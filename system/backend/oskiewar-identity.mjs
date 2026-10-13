import { handleFor, handleBySharedEmail, userEmailFromID } from './authorization.mjs';
import { accountLocked } from './account-lock.mjs';
import { connect } from './database.mjs';

// Password and email-code sign-ins can have separate Auth0 subjects. Resolve
// the AC handle's owner only after Auth0 confirms the same verified address.
// A cached handle label alone never grants access to another account's data.
export async function fighterIdentity(sub) {
  const database = await connect();
  const { db } = database;
  const handles = db.collection('@handles');
  const own = await handles.findOne({ _id: sub });
  if (own?.handle) return { sub, handle: own.handle };
  const label = await handleFor(sub);
  let owner = label && await handles.findOne({ handle: label.replace(/^@/, '') });
  let signedIn;
  if (!owner) {
    signedIn = await userEmailFromID(sub);
    if (!signedIn)
      throw Object.assign(Error('Could not verify your AC account. Try again.'), { status: 503 });
    // A passwordless subject can have no cached label at all. Resolve through
    // Auth0's same-tenant verified email lookup, then recheck both identities
    // below before using the existing handle. Do not merge or rewrite accounts.
    if (signedIn.email_verified === true && signedIn.email) {
      let sibling;
      try { sibling = await handleBySharedEmail({ ...signedIn, sub }, database); }
      catch { throw Object.assign(Error('Could not verify your AC account. Try again.'), { status: 503 }); }
      if (sibling?.sub) owner = await handles.findOne({ _id: sibling.sub });
    }
  }
  if (!owner) return { sub, handle: null };
  const [verifiedSignIn, primary] = await Promise.all([
    signedIn || userEmailFromID(sub), userEmailFromID(owner._id),
  ]);
  if (!verifiedSignIn || !primary)
    throw Object.assign(Error('Could not verify your AC account. Try again.'), { status: 503 });
  if (verifiedSignIn.email_verified !== true || primary.email_verified !== true ||
      !verifiedSignIn.email || verifiedSignIn.email.toLowerCase() !== primary.email?.toLowerCase())
    throw Object.assign(Error('This sign-in does not own that AC handle.'), { status: 403 });
  if (await accountLocked(owner._id))
    throw Object.assign(Error('This AC account is locked.'), { status: 401 });
  return { sub: owner._id, handle: owner.handle };
}
