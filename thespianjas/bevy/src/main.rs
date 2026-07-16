use bevy::prelude::*;
use std::collections::HashMap;

const MODEL: &str = "thespianjas/assets/versions/v001/model.glb#Scene0";
const AUDIO: &str = "marketing/talking-head/out/.work-yc/mono16k.wav";

#[derive(Resource, Default)]
struct SpeechEnvelope(Vec<f32>);

#[derive(Component)]
struct Performer;

fn main() {
    App::new()
        .insert_resource(ClearColor(Color::srgb(0.09, 0.075, 0.11)))
        .insert_resource(load_speech_envelope())
        .add_plugins(DefaultPlugins
            .set(AssetPlugin {
                file_path: concat!(env!("CARGO_MANIFEST_DIR"), "/../..").into(),
                ..default()
            })
            .set(WindowPlugin {
            primary_window: Some(Window {
                title: "thespianjas · Bevy Metal studio".into(),
                resolution: (720, 1280).into(),
                ..default()
            }),
            ..default()
            }))
        .add_systems(Startup, setup)
        .add_systems(Update, (fly_camera, soften_performer_material, perform_speech).chain())
        .run();
}

fn setup(
    mut commands: Commands,
    assets: Res<AssetServer>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    commands.spawn(AmbientLight {
        color: Color::srgb(0.48, 0.55, 0.50),
        brightness: 260.0,
        affects_lightmapped_meshes: true,
    });
    commands.spawn((
        Performer,
        SceneRoot(assets.load(MODEL)),
        // Meshy's canonical textured mesh is 1.82 units tall. Keep this
        // unwrapped model as the visual endpoint until the skinned export's
        // 0.01 armature hierarchy is normalized into a production face rig.
        Transform::from_xyz(0.0, 1.25, 0.0)
            .with_scale(Vec3::splat(2.4 / 1.90)),
    ));

    commands.spawn(AudioPlayer::new(assets.load(AUDIO)));

    // Compact Source-era interview room: a handful of strong architectural
    // planes and practicals, not an empty showroom or a high-detail set.
    let concrete = materials.add(StandardMaterial {
        base_color: Color::srgb(0.22, 0.23, 0.21),
        perceptual_roughness: 0.92,
        metallic: 0.0,
        ..default()
    });
    let dark = materials.add(StandardMaterial {
        base_color: Color::srgb(0.075, 0.085, 0.08),
        perceptual_roughness: 0.86,
        ..default()
    });
    let wall_mesh = meshes.add(Cuboid::new(4.2, 3.2, 0.12));
    commands.spawn((Mesh3d(wall_mesh), MeshMaterial3d(concrete.clone()), Transform::from_xyz(0.0, 1.5, -2.2)));
    commands.spawn((Mesh3d(meshes.add(Cuboid::new(0.16, 3.2, 3.8))), MeshMaterial3d(dark.clone()), Transform::from_xyz(-2.0, 1.5, -0.3)));
    commands.spawn((Mesh3d(meshes.add(Cuboid::new(0.16, 3.2, 3.8))), MeshMaterial3d(dark.clone()), Transform::from_xyz(2.0, 1.5, -0.3)));
    commands.spawn((Mesh3d(meshes.add(Cuboid::new(4.2, 0.10, 2.4))), MeshMaterial3d(concrete), Transform::from_xyz(0.0, -0.03, 0.1)));

    commands.spawn((
        DirectionalLight {
            illuminance: 7_500.0,
            shadows_enabled: false,
            color: Color::srgb(1.0, 0.91, 0.80),
            ..default()
        },
        Transform::from_xyz(-3.0, 4.0, 4.0).looking_at(Vec3::new(0.0, 1.2, 0.0), Vec3::Y),
    ));
    commands.spawn((
        PointLight {
            intensity: 500_000.0,
            color: Color::srgb(0.45, 0.82, 1.0),
            range: 12.0,
            ..default()
        },
        Transform::from_xyz(3.0, 2.0, 3.0),
    ));

    commands.spawn((
        Camera3d::default(),
        Projection::Perspective(PerspectiveProjection {
            fov: 24.0_f32.to_radians(),
            ..default()
        }),
        Transform::from_xyz(0.0, 2.05, 2.5)
            .looking_at(Vec3::new(0.0, 2.05, 0.0), Vec3::Y),
    ));
}

fn soften_performer_material(
    meshes: Query<(&Name, &MeshMaterial3d<StandardMaterial>)>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    for (name, handle) in &meshes {
        if name.as_str() != "char1" { continue; }
        if let Some(material) = materials.get_mut(&handle.0) {
            material.perceptual_roughness = 0.78;
            material.metallic = 0.0;
            material.reflectance = 0.24;
        }
    }
}

fn load_speech_envelope() -> SpeechEnvelope {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..").join(AUDIO);
    let Ok(mut reader) = hound::WavReader::open(&path) else {
        warn!("speech audio unavailable at {}", path.display());
        return default();
    };
    let spec = reader.spec();
    let channels = spec.channels.max(1) as usize;
    let bucket = (spec.sample_rate as usize / 30).max(1) * channels;
    let samples: Vec<f32> = reader
        .samples::<i16>()
        .filter_map(Result::ok)
        .map(|sample| sample as f32 / i16::MAX as f32)
        .collect();
    let mut envelope = Vec::with_capacity(samples.len() / bucket + 1);
    for frame in samples.chunks(bucket) {
        let rms = (frame.iter().map(|x| x * x).sum::<f32>() / frame.len().max(1) as f32).sqrt();
        envelope.push((rms * 7.0).clamp(0.0, 1.0));
    }
    info!("loaded {:.1}s speech envelope", envelope.len() as f32 / 30.0);
    SpeechEnvelope(envelope)
}

fn perform_speech(
    time: Res<Time>,
    envelope: Res<SpeechEnvelope>,
    mut originals: Local<HashMap<Entity, Quat>>,
    mut joints: Query<(Entity, &Name, &mut Transform)>,
    mut performer: Query<&mut Transform, (With<Performer>, Without<Name>)>,
) {
    if envelope.0.is_empty() { return; }
    let elapsed = time.elapsed_secs();
    let frame = (elapsed * 30.0) as usize;
    let raw = envelope.0.get(frame).copied().unwrap_or(0.0);
    let previous = envelope.0.get(frame.saturating_sub(2)).copied().unwrap_or(raw);
    let speech = raw * 0.7 + previous * 0.3;
    let sway = (elapsed * 1.7).sin();
    if let Ok(mut root) = performer.single_mut() {
        root.translation.y = 1.25 + speech * 0.012;
        root.rotation = Quat::from_rotation_z(sway * (0.004 + speech * 0.006))
            * Quat::from_rotation_y(sway * speech * 0.008);
    }
    for (entity, name, mut transform) in &mut joints {
        let weight = match name.as_str() {
            "Head" => 1.0,
            "neck" => 0.45,
            "Spine02" => 0.18,
            _ => continue,
        };
        let base = *originals.entry(entity).or_insert(transform.rotation);
        transform.rotation = base
            * Quat::from_rotation_x(-speech * 0.075 * weight)
            * Quat::from_rotation_y(sway * (0.012 + speech * 0.018) * weight);
    }
}

fn fly_camera(
    keys: Res<ButtonInput<KeyCode>>,
    time: Res<Time>,
    mut cameras: Query<&mut Transform, With<Camera3d>>,
) {
    let Ok(mut transform) = cameras.single_mut() else { return };
    let mut direction = Vec3::ZERO;
    if keys.pressed(KeyCode::KeyW) { direction += *transform.forward(); }
    if keys.pressed(KeyCode::KeyS) { direction += *transform.back(); }
    if keys.pressed(KeyCode::KeyA) { direction += *transform.left(); }
    if keys.pressed(KeyCode::KeyD) { direction += *transform.right(); }
    if keys.pressed(KeyCode::KeyQ) { direction -= Vec3::Y; }
    if keys.pressed(KeyCode::KeyE) { direction += Vec3::Y; }
    if direction != Vec3::ZERO {
        let speed = if keys.pressed(KeyCode::ShiftLeft) { 4.5 } else { 1.6 };
        transform.translation += direction.normalize() * speed * time.delta_secs();
    }
}
