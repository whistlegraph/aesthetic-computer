"""Read-only stage observations. Never retain full GPU activations or draw RNG."""
import time
import numpy as np


class ModelTrace:
    def __init__(self, pipe, emit):
        self.pipe, self.emit = pipe, emit
        self.handles = []
        self.started = time.perf_counter()
        self.step = 0
        self.frames = []
        self.previous = None

    def sample(self, stage, symbol, tensor=None, **extra):
        frame = {"index": len(self.frames), "stage": stage, "symbol": symbol,
                 "seconds": round(time.perf_counter()-self.started, 4),
                 "step": self.step, **extra}
        if tensor is not None:
            import torch.nn.functional as F
            x = tensor.detach().float()
            # RMS over channels, pooled only for transport. This is magnitude,
            # not attention, attribution, or a decoded image.
            magnitude = x[0].square().mean(dim=0).sqrt()
            h, w = magnitude.shape
            grid = F.adaptive_avg_pool2d(magnitude[None, None], (min(h, 32), min(w, 32)))[0, 0]
            values = grid.cpu().numpy()
            maximum = float(values.max())
            frame.update(shape=list(x.shape[1:]), grid_shape=list(values.shape),
                         grid=np.rint(values / max(maximum, 1e-8) * 255).astype(np.uint8).ravel().tolist(),
                         rms=float(np.sqrt(np.mean(values**2))), scale_max=maximum,
                         legend="Channel RMS · normalized per sample")
        frame["seconds"] = round(time.perf_counter()-self.started, 4)
        self.frames.append(frame)
        self.emit(frame)

    def __enter__(self):
        pipe = self.pipe
        self.original_add_noise = pipe.scheduler.add_noise
        self.had_override = "add_noise" in vars(pipe.scheduler)
        self.original_override = vars(pipe.scheduler).get("add_noise")

        def add_noise(original_samples, noise, timesteps):
            self.sample("Latent", "z = E(x)", original_samples)
            self.sample("Noise", "ε", noise)
            result = self.original_add_noise(original_samples, noise, timesteps)
            self.previous = result.detach().clone()
            self.sample("Noisy latent", "zₜ = z + σε", result,
                        timestep=float(timesteps[0].item()))
            return result

        pipe.scheduler.add_noise = add_noise
        self.handles.append(pipe.vae.encoder.register_forward_pre_hook(
            lambda module, args: self.sample("Encode", "E(x)")))

        def unet_start(module, args):
            self.step += 1
            self.sample("Network input", "U(zₜ, t)", args[0], timestep=float(args[1].item()))

        self.handles.append(pipe.unet.register_forward_pre_hook(unet_start))
        for i, block in enumerate(pipe.unet.down_blocks):
            def down(module, args, output, i=i):
                self.sample(f"Down {i+1}", f"↓{i+1}", output[0])
            self.handles.append(block.register_forward_hook(down))
        self.handles.append(pipe.unet.mid_block.register_forward_hook(
            lambda module, args, output: self.sample("Middle", "↔", output)))
        for i, block in enumerate(pipe.unet.up_blocks):
            def up(module, args, output, i=i):
                self.sample(f"Up {i+1}", f"↑{i+1}", output)
            self.handles.append(block.register_forward_hook(up))
        prediction = pipe.scheduler.config.prediction_type
        self.handles.append(pipe.unet.register_forward_hook(
            lambda module, args, output: self.sample("Prediction", "Uθ", output[0],
                                                      prediction_type=prediction)))
        self.handles.append(pipe.vae.decoder.register_forward_pre_hook(
            lambda module, args: self.sample("Decode", "D(z′)")))
        return self

    def callback(self, pipe, index, timestep, values):
        latent = values["latents"]
        if self.previous is not None:
            self.sample("Latent change", "Δz", latent-self.previous)
        self.sample("Denoised latent", "z′", latent, timestep=float(timestep.item()))
        self.previous = latent.detach().clone()
        return values

    def __exit__(self, kind, value, traceback):
        for handle in self.handles:
            handle.remove()
        if self.had_override:
            self.pipe.scheduler.add_noise = self.original_override
        else:
            del self.pipe.scheduler.add_noise
        self.previous = None
