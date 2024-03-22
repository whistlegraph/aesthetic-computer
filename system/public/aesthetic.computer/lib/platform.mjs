let nav;

try {
  nav = navigator;
} catch (e) {
  nav = {};
}

export const iOS = /(iPad|iPhone|iPod)/g.test(nav.userAgent);
export const Safari = /apple/i.test(nav.vendor);
export const Android = /(Android)/g.test(nav.userAgent);
export const MetaBrowser = /(OculusBrowser)/g.test(nav.userAgent);
export const Desktop = !iOS && !Android && !MetaBrowser;
export const Instagram = /(Instagram)/g.test(nav.userAgent);
export const TikTok = /BytedanceWebview/i.test(nav.userAgent);
export const Aesthetic = /Aesthetic/i.test(nav.userAgent);
export const AestheticExtension = /AestheticExtension/i.test(nav.userAgent);