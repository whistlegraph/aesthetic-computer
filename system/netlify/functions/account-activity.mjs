import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { createAccountActivityHandler } from "../../backend/account-activity-handler.mjs";

export const handler = createAccountActivityHandler({ authorize, connect });
