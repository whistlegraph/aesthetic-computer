export const iOS = /(iPad|iPhone|iPod)/g.test(navigator.userAgent);
export const Safari = /apple/i.test(navigator.vendor);
export const Android = /(Android)/g.test(navigator.userAgent);
export const MetaBrowser = /(OculusBrowser)/g.test(navigator.userAgent);
export const Desktop = !iOS && !Android && !MetaBrowser;
export const Instagram = /(Instagram)/g.test(navigator.userAgent);
