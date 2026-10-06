import { handleFor, userEmailFromID } from './authorization.mjs';
import { accountLocked } from './account-lock.mjs';
import { connect } from './database.mjs';

// Password and email-code sign-ins can have separate Auth0 subjects. Resolve
// the AC handle's owner only after Auth0 confirms the same verified address.
// A cached handle label alone never grants access to another account's data.
export async function fighterIdentity(sub) {
  const { db } = await connect();
  const handles = db.collection('@handles');
  const own = await handles.findOne({ _id: sub });
  if (own?.handle) return { sub, handle: own.handle };
  const label = await handleFor(sub);
  const owner = label && await handles.findOne({ handle: label.replace(/^@/, '') });
  if (!owner) return { sub, handle: null };
  const [signedIn, primary] = await Promise.all([
    userEmailFromID(sub), userEmailFromID(owner._id),
  ]);
  if (!signedIn || !primary)
    throw Object.assign(Error('Could not verify your AC account. Try again.'), { status: 503 });
  if (signedIn.email_verified !== true || primary.email_verified !== true ||
      !signedIn.email || signedIn.email.toLowerCase() !== primary.email?.toLowerCase())
    throw Object.assign(Error('This sign-in does not own that AC handle.'), { status: 403 });
  if (await accountLocked(owner._id))
    throw Object.assign(Error('This AC account is locked.'), { status: 401 });
  return { sub: owner._id, handle: owner.handle };
}
