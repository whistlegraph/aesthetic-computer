// Compatibility route for installed clients; Whistlegraph is canonical.
export {handler,authenticateWhistlegraph as authenticateWalkieware,whistlegraphStore as walkiewareStore} from './whistlegraph.mjs';
