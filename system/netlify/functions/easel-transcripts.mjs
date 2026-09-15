import { authorize } from '../../backend/authorization.mjs';
import { connect } from '../../backend/database.mjs';
import { createTranscriptHandler } from '../../backend/easel-transcripts.mjs';
export const handler=createTranscriptHandler({authorize,connect});
