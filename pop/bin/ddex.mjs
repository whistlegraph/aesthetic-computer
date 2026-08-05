#!/usr/bin/env node

import { createHash } from "node:crypto";
import { copyFile, mkdir, readFile, stat, writeFile } from "node:fs/promises";
import { homedir } from "node:os";
import { basename, dirname, isAbsolute, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const ROOT = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const RELEASES = join(ROOT, "pop/releases");
const DEFAULT_OUT = join(ROOT, "pop/out/ddex");

const DRAFT = Object.freeze({
  senderDpid: "PADPIDA0000000000X",
  recipientDpid: "PADPIDA0000000000X",
  isrc: "USZZZ2600001",
  icpn: "0000000000000",
  pLine: "2026 EVALUATION ONLY — NOT FOR DELIVERY",
  cLine: "2026 EVALUATION ONLY — NOT FOR DELIVERY",
});

function expandHome(value) {
  if (typeof value !== "string") return value;
  return value === "~" ? homedir() : value.startsWith("~/") ? join(homedir(), value.slice(2)) : value;
}

function deepMerge(base, overlay) {
  if (!overlay || typeof overlay !== "object" || Array.isArray(overlay)) return overlay ?? base;
  const output = { ...(base ?? {}) };
  for (const [key, value] of Object.entries(overlay)) {
    output[key] = value && typeof value === "object" && !Array.isArray(value)
      ? deepMerge(output[key], value)
      : value;
  }
  return output;
}

async function readJson(path) {
  return JSON.parse(await readFile(path, "utf8"));
}

async function exists(path) {
  try {
    return (await stat(path)).isFile();
  } catch {
    return false;
  }
}

export async function loadRelease(slug, privatePath) {
  const publicPath = join(RELEASES, slug, "release.json");
  const record = await readJson(publicPath);
  const overlayPath = expandHome(privatePath ?? record.privateOverlay?.defaultPath);
  const overlayFound = Boolean(overlayPath && await exists(overlayPath));
  const overlay = overlayFound ? await readJson(overlayPath) : {};
  return { record: deepMerge(record, overlay), publicPath, overlayPath, overlayFound };
}

function get(object, path) {
  return path.split(".").reduce((value, key) => value?.[key], object);
}

export async function collectIssues(record, { requireAssets = true } = {}) {
  const issues = [];
  const required = [
    ["message.sender.dpid", "Sender DPID", "Register the sending party with DDEX."],
    ["message.sender.fullName", "Sender legal name", "Set the legal sender name in the private overlay."],
    ["message.recipient.dpid", "Recipient DPID", "Obtain the DSP or distributor's DPID and exchange rules."],
    ["message.recipient.fullName", "Recipient name", "Set the partner name in the private overlay."],
    ["identifiers.isrc", "ISRC", "Copy the recording ISRC from the distributor release record."],
    ["identifiers.icpn", "ICPN / UPC / EAN", "Copy the release barcode from the distributor release record."],
    ["rights.pLine.year", "P-line year", "Confirm the phonogram-rights year."],
    ["rights.pLine.text", "P-line text", "Confirm the phonogram-rights owner and wording."],
    ["rights.cLine.year", "C-line year", "Confirm the release copyright year."],
    ["rights.cLine.text", "C-line text", "Confirm the release copyright owner and wording."],
  ];
  for (const [path, label, action] of required) {
    if (get(record, path) === undefined || get(record, path) === null || get(record, path) === "") {
      issues.push({ severity: "blocker", path, label, action });
    }
  }

  if (requireAssets) {
    for (const kind of ["audio", "cover"]) {
      const path = expandHome(record.assets?.[kind]?.localPath);
      if (!path || !(await exists(path))) {
        issues.push({
          severity: "blocker",
          path: `assets.${kind}.localPath`,
          label: `${kind === "audio" ? "Master audio" : "Cover art"} file`,
          action: `Restore or point to the local ${kind} delivery asset.`,
        });
      }
    }
  }

  issues.push({
    severity: "notice",
    path: "ddex.implementationLicence",
    label: "DDEX implementation licence",
    action: "Keep this in evaluation until the licence, DPID, and partner agreement are in place.",
  });
  return issues;
}

function xml(value) {
  return String(value ?? "")
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;")
    .replaceAll("'", "&apos;");
}

export function formatDuration(totalSeconds) {
  const seconds = Math.max(0, Number(totalSeconds));
  const hours = Math.floor(seconds / 3600);
  const minutes = Math.floor((seconds % 3600) / 60);
  const remainder = Number((seconds % 60).toFixed(3));
  return `PT${hours ? `${hours}H` : ""}${minutes ? `${minutes}M` : ""}${remainder}S`;
}

function makeIds(record, draft) {
  return {
    senderDpid: record.message?.sender?.dpid ?? (draft ? DRAFT.senderDpid : ""),
    senderName: record.message?.sender?.fullName ?? (draft ? "Aesthetic Computer — evaluation sender" : ""),
    recipientDpid: record.message?.recipient?.dpid ?? (draft ? DRAFT.recipientDpid : ""),
    recipientName: record.message?.recipient?.fullName ?? (draft ? "Evaluation recipient" : ""),
    isrc: record.identifiers?.isrc ?? (draft ? DRAFT.isrc : ""),
    icpn: record.identifiers?.icpn ?? (draft ? DRAFT.icpn : ""),
    pYear: record.rights?.pLine?.year ?? new Date(record.release.releaseDate).getUTCFullYear(),
    pLine: record.rights?.pLine?.text ?? (draft ? DRAFT.pLine : ""),
    cYear: record.rights?.cLine?.year ?? new Date(record.release.releaseDate).getUTCFullYear(),
    cLine: record.rights?.cLine?.text ?? (draft ? DRAFT.cLine : ""),
  };
}

function deliveryFile(asset, kind) {
  if (!asset?.packetUri) return "";
  const hash = asset.md5
    ? `\n                  <HashSum>\n                     <Algorithm>MD5</Algorithm>\n                     <HashSumValue>${xml(asset.md5)}</HashSumValue>\n                  </HashSum>`
    : "";
  if (kind === "audio") {
    return `
            <TechnicalDetails>
               <TechnicalResourceDetailsReference>T1</TechnicalResourceDetailsReference>
               <DeliveryFile>
                  <Type>AudioFile</Type>
                  <ContainerFormat>${xml(asset.containerFormat)}</ContainerFormat>
                  <AudioCodecType>${xml(asset.audioCodecType)}</AudioCodecType>
                  <NumberOfChannels>${xml(asset.numberOfChannels)}</NumberOfChannels>
                  <SamplingRate>${xml(asset.samplingRate)}</SamplingRate>
                  <BitsPerSample>${xml(asset.bitsPerSample)}</BitsPerSample>
                  <File>
                     <URI>${xml(asset.packetUri)}</URI>${hash}
                  </File>
               </DeliveryFile>
            </TechnicalDetails>`;
  }
  return `
         <TechnicalDetails>
            <TechnicalResourceDetailsReference>T2</TechnicalResourceDetailsReference>
            <ImageCodecType>${xml(asset.imageCodecType)}</ImageCodecType>
            <ImageHeight>${xml(asset.height)}</ImageHeight>
            <ImageWidth>${xml(asset.width)}</ImageWidth>
            <File>
               <URI>${xml(asset.packetUri)}</URI>${hash}
            </File>
         </TechnicalDetails>`;
}

export function buildErnXml(record, { draft = false, createdAt = new Date(), messageId } = {}) {
  const ids = makeIds(record, draft);
  const r = record.release;
  const a = record.recording;
  const art = record.artwork;
  const namespace = record.ddex.namespace;
  const id = messageId ?? `AC-${record.slug}-${createdAt.toISOString().replace(/[-:.]/g, "")}`;
  const control = draft ? "TestMessage" : "LiveMessage";
  const notice = draft
    ? "\n   <!-- EVALUATION ONLY: TestMessage with conspicuous placeholder identifiers. NOT FOR DELIVERY. -->"
    : "";
  const deals = r.deals.map((deal) => `
         <Deal>
            <DealTerms>
               <TerritoryCode>${xml(r.territoryCode)}</TerritoryCode>
               <ValidityPeriod><StartDate>${xml(r.releaseDate)}</StartDate></ValidityPeriod>
               <CommercialModelType>${xml(deal.commercialModelType)}</CommercialModelType>${deal.useTypes.map((use) => `\n               <UseType>${xml(use)}</UseType>`).join("")}
            </DealTerms>
         </Deal>`).join("");

  return `<?xml version="1.0" encoding="UTF-8"?>
<ern:NewReleaseMessage xmlns:ern="${xml(namespace)}"
   xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
   xsi:schemaLocation="${xml(namespace)} ${xml(namespace)}/release-notification.xsd"
   ReleaseProfileVersionId="${xml(record.ddex.releaseProfileVersionId)}"
   LanguageAndScriptCode="en" AvsVersionId="${xml(record.ddex.avsVersionId)}">${notice}
   <MessageHeader>
      <MessageThreadId>${xml(id)}</MessageThreadId>
      <MessageId>${xml(id)}</MessageId>
      <MessageSender><PartyId>${xml(ids.senderDpid)}</PartyId><PartyName><FullName>${xml(ids.senderName)}</FullName></PartyName></MessageSender>
      <MessageRecipient><PartyId>${xml(ids.recipientDpid)}</PartyId><PartyName><FullName>${xml(ids.recipientName)}</FullName></PartyName></MessageRecipient>
      <MessageCreatedDateTime>${createdAt.toISOString()}</MessageCreatedDateTime>
      <MessageControlType>${control}</MessageControlType>
   </MessageHeader>
   <PartyList>
      <Party>
         <PartyReference>PMainArtist</PartyReference>
         <PartyName><FullName>${xml(r.displayArtist)}</FullName></PartyName>
      </Party>
      <Party>
         <PartyReference>PLabel</PartyReference>
         <PartyName><FullName>${xml(r.labelName)}</FullName></PartyName>
      </Party>
   </PartyList>
   <ResourceList>
      <SoundRecording>
         <ResourceReference>A1</ResourceReference>
         <Type>${xml(a.type)}</Type>
         <SoundRecordingEdition>
            <ResourceId><ISRC>${xml(ids.isrc)}</ISRC></ResourceId>
            <PLine><Year>${xml(ids.pYear)}</Year><PLineText>${xml(ids.pLine)}</PLineText></PLine>${deliveryFile(record.assets.audio, "audio")}
         </SoundRecordingEdition>
         <DisplayTitleText>${xml(a.title)}</DisplayTitleText>
         <DisplayTitle ApplicableTerritoryCode="Worldwide"><TitleText>${xml(a.title)}</TitleText></DisplayTitle>
         <DisplayArtistName ApplicableTerritoryCode="Worldwide">${xml(r.displayArtist)}</DisplayArtistName>
         <DisplayArtist SequenceNumber="1"><ArtistPartyReference>PMainArtist</ArtistPartyReference><DisplayArtistRole>MainArtist</DisplayArtistRole></DisplayArtist>
         <Contributor SequenceNumber="1"><ContributorPartyReference>PMainArtist</ContributorPartyReference><Role><Value>Artist</Value></Role></Contributor>
         <Duration>${formatDuration(a.durationSeconds)}</Duration>
         <ParentalWarningType>${xml(r.parentalWarningType)}</ParentalWarningType>
         <ContainsAI>${xml(a.containsAI)}</ContainsAI>
         <HasVocalPerformance>${xml(a.hasVocalPerformance)}</HasVocalPerformance>
         <HasForegroundVocalPerformance>${xml(a.hasForegroundVocalPerformance)}</HasForegroundVocalPerformance>
      </SoundRecording>
      <Image>
         <ResourceReference>A2</ResourceReference>
         <Type>${xml(art.type)}</Type>
         <ResourceId><ProprietaryId Namespace="${xml(ids.senderDpid)}">${xml(ids.icpn)}-COVER</ProprietaryId></ResourceId>
         <ContainsAI>${xml(art.containsAI)}</ContainsAI>${deliveryFile(record.assets.cover, "cover")}
      </Image>
   </ResourceList>
   <ReleaseList>
      <Release>
         <ReleaseReference>R0</ReleaseReference>
         <ReleaseType>SingleResourceRelease</ReleaseType>
         <ReleaseId><ICPN>${xml(ids.icpn)}</ICPN><ProprietaryId Namespace="${xml(ids.senderDpid)}">${xml(record.slug)}</ProprietaryId></ReleaseId>
         <DisplayTitleText>${xml(r.title)}</DisplayTitleText>
         <DisplayTitle ApplicableTerritoryCode="Worldwide"><TitleText>${xml(r.title)}</TitleText></DisplayTitle>
         <DisplayArtistName ApplicableTerritoryCode="Worldwide">${xml(r.displayArtist)}</DisplayArtistName>
         <DisplayArtist SequenceNumber="1"><ArtistPartyReference>PMainArtist</ArtistPartyReference><DisplayArtistRole>MainArtist</DisplayArtistRole></DisplayArtist>
         <ReleaseLabelReference ApplicableTerritoryCode="Worldwide" IsDefault="true">PLabel</ReleaseLabelReference>
         <PLine><Year>${xml(ids.pYear)}</Year><PLineText>${xml(ids.pLine)}</PLineText></PLine>
         <CLine><Year>${xml(ids.cYear)}</Year><CLineText>${xml(ids.cLine)}</CLineText></CLine>
         <DisplayGenre ApplicableTerritoryCode="Worldwide"><GenreText>${xml(r.genre)}</GenreText><SubGenre>${xml(r.subGenre)}</SubGenre></DisplayGenre>
         <ParentalWarningType>${xml(r.parentalWarningType)}</ParentalWarningType>
         <ResourceGroup><SequenceNumber>1</SequenceNumber><ResourceGroupContentItem><SequenceNumber>1</SequenceNumber><ReleaseResourceReference>A1</ReleaseResourceReference><LinkedReleaseResourceReference>A2</LinkedReleaseResourceReference></ResourceGroupContentItem></ResourceGroup>
      </Release>
   </ReleaseList>
   <DealList>
      <ReleaseDeal>
         <DealReleaseReference>R0</DealReleaseReference>${deals}
      </ReleaseDeal>
   </DealList>
</ern:NewReleaseMessage>
`;
}

async function md5(path) {
  return createHash("md5").update(await readFile(path)).digest("hex");
}

async function prepareAsset(asset, outputDirectory, draft) {
  const localPath = expandHome(asset.localPath);
  if (!draft && await exists(localPath)) {
    const target = join(outputDirectory, "resources", asset.fileName ?? basename(localPath));
    await mkdir(dirname(target), { recursive: true });
    await copyFile(localPath, target);
    return { ...asset, packetUri: `resources/${basename(target)}`, md5: await md5(target) };
  }
  return { ...asset, packetUri: undefined, md5: undefined };
}

export async function exportPacket(record, { draft = false, out } = {}) {
  const outputDirectory = resolve(out ?? join(DEFAULT_OUT, record.slug));
  await mkdir(outputDirectory, { recursive: true });
  const prepared = deepMerge(record, { assets: {
    audio: await prepareAsset(record.assets.audio, outputDirectory, draft),
    cover: await prepareAsset(record.assets.cover, outputDirectory, draft),
  }});
  const createdAt = new Date();
  const xmlText = buildErnXml(prepared, { draft, createdAt });
  const xmlPath = join(outputDirectory, "NewReleaseMessage.xml");
  await writeFile(xmlPath, xmlText, "utf8");
  const issues = await collectIssues(record, { requireAssets: !draft });
  const receipt = {
    mode: draft ? "evaluation" : "live",
    deliveryAuthorized: !draft,
    standard: "DDEX ERN 4.3.2 / Simple Audio Single",
    createdAt: createdAt.toISOString(),
    xml: basename(xmlPath),
    issues,
  };
  await writeFile(join(outputDirectory, "receipt.json"), `${JSON.stringify(receipt, null, 2)}\n`, "utf8");
  return { outputDirectory, xmlPath, receipt };
}

function parseArgs(argv) {
  const [command, slug, ...rest] = argv;
  const options = { command, slug, draft: false };
  for (let i = 0; i < rest.length; i += 1) {
    const arg = rest[i];
    if (arg === "--draft") options.draft = true;
    else if (arg === "--json") options.json = true;
    else if (arg === "--private") options.privatePath = rest[++i];
    else if (arg === "--out") options.out = rest[++i];
    else throw new Error(`Unknown option: ${arg}`);
  }
  return options;
}

function printIssues(slug, issues, overlayPath, overlayFound) {
  const blockers = issues.filter((issue) => issue.severity === "blocker");
  console.log(`${slug}: ${blockers.length ? `${blockers.length} blockers` : "ready for packet export"}`);
  console.log(`private overlay: ${overlayFound ? "loaded" : "not found"} (${overlayPath})`);
  for (const issue of issues) console.log(`${issue.severity.toUpperCase().padEnd(7)} ${issue.label}: ${issue.action}`);
}

async function main() {
  const options = parseArgs(process.argv.slice(2));
  if (!options.command || !options.slug || !["check", "ern"].includes(options.command)) {
    console.error("Usage: node pop/bin/ddex.mjs <check|ern> <slug> [--private path] [--draft] [--out dir] [--json]");
    process.exitCode = 2;
    return;
  }
  const loaded = await loadRelease(options.slug, options.privatePath);
  const issues = await collectIssues(loaded.record, { requireAssets: true });
  const blockers = issues.filter((issue) => issue.severity === "blocker");
  if (options.command === "check") {
    if (options.json) console.log(JSON.stringify({
      slug: options.slug,
      publicPath: loaded.publicPath,
      overlayPath: loaded.overlayPath,
      overlayFound: loaded.overlayFound,
      issues,
    }, null, 2));
    else printIssues(options.slug, issues, loaded.overlayPath, loaded.overlayFound);
    process.exitCode = blockers.length ? 1 : 0;
    return;
  }
  if (blockers.length && !options.draft) {
    printIssues(options.slug, issues, loaded.overlayPath, loaded.overlayFound);
    console.error("Live export refused. Resolve blockers or use --draft for an evaluation TestMessage.");
    process.exitCode = 1;
    return;
  }
  const result = await exportPacket(loaded.record, options);
  console.log(`${options.draft ? "Evaluation" : "Live"} packet: ${result.outputDirectory}`);
  console.log(`XML: ${result.xmlPath}`);
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  main().catch((error) => {
    console.error(error.stack ?? error.message);
    process.exitCode = 1;
  });
}
