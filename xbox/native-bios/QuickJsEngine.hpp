#pragma once
#include "../runtime/include/ac/runtime.hpp"

namespace ac::xbox {
class QuickJsEngine final : public JsEngine {
 public:
  std::unique_ptr<JsPiece> compile(const PieceBundle&, const JsLimits&,
                                   std::string& error) override;
};
}  // namespace ac::xbox
