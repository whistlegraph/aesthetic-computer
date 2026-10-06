"""Read-only tracing, including cleanup when inference fails."""
from types import SimpleNamespace
import unittest
import torch
from model_trace import ModelTrace


class Scheduler:
    config = SimpleNamespace(prediction_type="epsilon")

    def add_noise(self, original_samples, noise, timesteps):
        return original_samples + noise


class TraceChecks(unittest.TestCase):
    def pipe(self):
        unet = torch.nn.Identity()
        unet.down_blocks = torch.nn.ModuleList([torch.nn.Identity()])
        unet.mid_block = torch.nn.Identity()
        unet.up_blocks = torch.nn.ModuleList([torch.nn.Identity()])
        return SimpleNamespace(unet=unet, scheduler=Scheduler(),
                               vae=SimpleNamespace(encoder=torch.nn.Identity(), decoder=torch.nn.Identity()))

    def test_noise_and_callback_are_unchanged_and_do_not_draw_rng(self):
        pipe, frames = self.pipe(), []
        latent = torch.arange(64, dtype=torch.float32).reshape(1,4,4,4)
        noise, timestep = torch.ones_like(latent), torch.tensor([250.])
        rng = torch.get_rng_state().clone()
        with ModelTrace(pipe, frames.append) as trace:
            result = pipe.scheduler.add_noise(latent, noise, timestep)
            self.assertTrue(torch.equal(result, latent+noise))
            values = {"latents": latent}
            self.assertIs(trace.callback(pipe, 0, timestep[0], values), values)
            self.assertIs(values["latents"], latent)
        self.assertTrue(torch.equal(rng, torch.get_rng_state()))
        self.assertNotIn("add_noise", vars(pipe.scheduler))
        self.assertEqual([f["stage"] for f in frames],
                         ["Latent", "Noise", "Noisy latent", "Latent change", "Denoised latent"])
        self.assertTrue(all(len(f["grid"]) == 16 for f in frames))
        self.assertTrue(all(0 <= v <= 255 for f in frames for v in f["grid"]))

    def test_failure_removes_hooks_and_restores_scheduler(self):
        pipe = self.pipe()
        with self.assertRaisesRegex(RuntimeError, "inference failed"):
            with ModelTrace(pipe, lambda frame: None):
                raise RuntimeError("inference failed")
        self.assertNotIn("add_noise", vars(pipe.scheduler))
        for module in [*pipe.unet.modules(), pipe.vae.encoder, pipe.vae.decoder]:
            self.assertFalse(module._forward_hooks)
            self.assertFalse(module._forward_pre_hooks)


if __name__ == "__main__":
    unittest.main()
