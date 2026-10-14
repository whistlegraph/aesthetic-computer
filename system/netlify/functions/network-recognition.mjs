import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { createNetworkRecognitionHandler } from "../../backend/network-recognition-handler.mjs";

export const handler = createNetworkRecognitionHandler({ authorize, connect });
