#include "ac/runtime.hpp"
#include <cassert>
#include <memory>
#include <stdexcept>
using namespace ac::xbox;
namespace {
class NullGraphics final : public Graphics { public: void wipe(Color) override {} void box(const Rect&) override {} void line(const Line&) override {} void write(const Text&) override {} };
class NullSound final : public Sound { public: void synth(const SynthVoice&) override {} void stop_all() override {} int sample_rate() const override { return 48000; } };
class FakePiece final : public JsPiece {
 public:
  FakePiece(std::string s, std::string v, bool fail) : slug_(std::move(s)), version_(std::move(v)), fail_(fail) {}
  std::string_view slug() const noexcept override { return slug_; }
  std::string_view version() const noexcept override { return version_; }
  void boot(Api&) override { if (fail_) throw std::runtime_error("boot"); }
  void sim(Api&) override {} void paint(Api&) override {} void act(Api&, const Event&) override {}
 private: std::string slug_, version_; bool fail_;
};
class FakeEngine final : public JsEngine { public: std::unique_ptr<JsPiece> compile(const PieceBundle& b, const JsLimits&, std::string& error) override { if (b.source == "syntax error") { error = "compile failed"; return {}; } return std::make_unique<FakePiece>(b.slug, b.version, b.source == "boot error"); } };
}
int main() {
  NullGraphics graphics; NullSound sound; Api api{{}, {}, {}, {}, graphics, sound};
  FakeEngine engine; PieceSupervisor supervisor(engine, JsLimits{64}); std::string error;
  assert(supervisor.stage({"fight", "v1", "ok", "hash"}, api, error)); assert(supervisor.activate(api)); assert(supervisor.generation() == 1);
  assert(!supervisor.stage({"fight", "bad", "syntax error", "hash"}, api, error)); assert(supervisor.generation() == 1);
  assert(!supervisor.stage({"fight", "bad", "boot error", "hash"}, api, error));
  assert(supervisor.stage({"fight", "v2", "ok", "hash"}, api, error)); assert(supervisor.activate(api)); assert(supervisor.generation() == 2);
  assert(supervisor.rollback(api)); assert(supervisor.generation() == 3);
  assert(!supervisor.stage({"fight", "huge", std::string(65, 'x'), "hash"}, api, error));
}
