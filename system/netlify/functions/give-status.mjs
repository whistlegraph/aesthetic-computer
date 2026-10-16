import { authorize } from "../../backend/authorization.mjs";
import { createGiveStatusHandler } from "../../backend/monthly-support.mjs";

export const handler = createGiveStatusHandler({ authorize });
