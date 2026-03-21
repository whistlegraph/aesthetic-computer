%global app_name    Aesthetic Computer
%global app_id      aesthetic-computer
%global install_dir /opt/aesthetic-computer

Name:           %{app_id}
Version:        %{rpm_version}
Release:        1%{?dist}
Summary:        A runtime and social network for creative computing

License:        MIT
URL:            https://aesthetic.computer
Source0:        %{app_id}-%{version}-linux-unpacked.tar.gz
Source1:        %{app_id}.desktop
Source2:        %{app_id}-icons.tar.gz

ExclusiveArch:  x86_64

# Electron runtime dependencies (Fedora package names)
Requires:       gtk3
Requires:       libnotify
Requires:       nss
Requires:       libXScrnSaver
Requires:       libXtst
Requires:       xdg-utils
Requires:       at-spi2-core
Requires:       libuuid
Requires:       mesa-libGL
Requires:       alsa-lib

# Don't strip Electron binaries or scan for auto-requires/provides
%global __strip /bin/true
%global __provides_exclude_from %{install_dir}/.*
%global __requires_exclude_from %{install_dir}/.*
AutoReqProv:    no

%description
Aesthetic Computer is a mobile-first runtime and social network for
creative computing. It provides a musical instrument-like interface
where users discover memorizable paths through commands and published
interactive programs called "pieces".

%prep
%setup -q -n linux-unpacked
%setup -q -T -D -a 2 -n linux-unpacked

%install
# Install application to /opt/aesthetic-computer
mkdir -p "%{buildroot}%{install_dir}"
cp -a . "%{buildroot}%{install_dir}/"

# Remove icon source dir from install (we place them properly below)
rm -rf "%{buildroot}%{install_dir}/icons"

# Desktop entry
mkdir -p "%{buildroot}%{_datadir}/applications"
install -Dm644 %{SOURCE1} "%{buildroot}%{_datadir}/applications/%{app_id}.desktop"

# Icons
for size in 16 24 32 48 64 128 256 512; do
    icon_dir="%{buildroot}%{_datadir}/icons/hicolor/${size}x${size}/apps"
    mkdir -p "$icon_dir"
    if [ -f "icons/${size}x${size}.png" ]; then
        install -Dm644 "icons/${size}x${size}.png" "$icon_dir/%{app_id}.png"
    fi
done

# Symlink to /usr/bin
mkdir -p "%{buildroot}%{_bindir}"
ln -sf "%{install_dir}/%{app_id}" "%{buildroot}%{_bindir}/%{app_id}"

%post
update-desktop-database %{_datadir}/applications &>/dev/null || :
touch --no-create %{_datadir}/icons/hicolor &>/dev/null || :
gtk-update-icon-cache %{_datadir}/icons/hicolor &>/dev/null || :

%postun
update-desktop-database %{_datadir}/applications &>/dev/null || :
touch --no-create %{_datadir}/icons/hicolor &>/dev/null || :
gtk-update-icon-cache %{_datadir}/icons/hicolor &>/dev/null || :

%files
%{install_dir}/
%{_bindir}/%{app_id}
%{_datadir}/applications/%{app_id}.desktop
%{_datadir}/icons/hicolor/*/apps/%{app_id}.png
