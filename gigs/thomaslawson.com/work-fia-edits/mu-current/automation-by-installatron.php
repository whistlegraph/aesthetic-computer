<?php
/*
  Plugin Name: WordPress automation by Installatron
  Description: Management of this WordPress site is provided by Installatron.
*/

// This plugin was added because Automatic Updates, Automatic Backups, and
// other features are managed externally by Installatron.

// To block this plugin, create an empty file in the same directory named:
//     block-automation-by-installatron.php

// Turn off WordPress automatic updates since these are managed externally.
// If you remove this to re-enable WordPress's automatic updates then it's 
// advised to disable auto-updating in Installatron.
add_filter( 'automatic_updater_disabled', '__return_true', -9999 );

// Disable WordPress site health test for Automatic Updates since these are
// managed externally by Installatron.
function installatron_filter_site_status_tests($tests) {
	unset($tests['async']['background_updates']);
	return $tests;
}
add_filter('site_status_tests', 'installatron_filter_site_status_tests');

