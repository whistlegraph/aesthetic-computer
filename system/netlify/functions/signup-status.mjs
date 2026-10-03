import { authorize, userEmailFromID, handleFor, forgetAuthorizations, resendVerificationEmail } from "../../backend/authorization.mjs";
import { createSignupStatusHandler } from "../../backend/signup-status.mjs";
export const handler = createSignupStatusHandler({ authorize, userEmailFromID, handleFor, forgetAuthorizations, resendVerificationEmail });
