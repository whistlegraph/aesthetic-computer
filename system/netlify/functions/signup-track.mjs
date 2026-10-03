import { connect } from "../../backend/database.mjs";
import { createSignupTrackingHandler } from "../../backend/signup-tracking.mjs";
export const handler = createSignupTrackingHandler({ connect });
