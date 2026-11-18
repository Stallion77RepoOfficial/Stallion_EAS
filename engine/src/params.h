#pragma once

#include "defs.h"

#include <algorithm>
#include <cmath>
#include <string>
#include <vector>

struct Parameter {
  std::string name;
  int &value;
  int min;
  int max;
};

inline std::vector<Parameter> params;
inline MultiArray<int, MaxSearchDepth + 1, MaxActions> LMRTable{};

// Internal eval units corresponding to 100 centipawns for UCI display.
inline int NormalizationFactor = 195;

class TunableParam {
  int current_value;

public:
  TunableParam(const char *label, int initial, int lo, int hi)
      : current_value(initial) {
    params.push_back(Parameter{label, current_value, lo, hi});
  }

  operator int() const noexcept { return current_value; }
};

#define ENGINE_PARAM(sym, lo, hi, def_val)                                     \
  inline TunableParam sym { #sym, def_val, lo, hi }

ENGINE_PARAM(NMPMinDepth, 1, 5, 1);
ENGINE_PARAM(NMPBase, 1, 5, 5);
ENGINE_PARAM(NMPDepthDiv, 3, 9, 9);
ENGINE_PARAM(NMPEvalDiv, 50, 300, 209);
ENGINE_PARAM(RFPMargin, 50, 150, 70);
ENGINE_PARAM(RFPMaxDepth, 6, 14, 10);
ENGINE_PARAM(LMRBase, 1, 8, 4);
ENGINE_PARAM(LMRRatio, 15, 30, 16);
ENGINE_PARAM(LMPBase, 1, 5, 4);
ENGINE_PARAM(LMPDepth, 3, 10, 8);
ENGINE_PARAM(SEDepth, 4, 10, 7);
ENGINE_PARAM(SEDoubleExtMargin, 10, 30, 14);
ENGINE_PARAM(FPDepth, 4, 9, 6);
ENGINE_PARAM(FPMargin1, 50, 150, 90);
ENGINE_PARAM(FPMargin2, 50, 150, 80);
ENGINE_PARAM(IIRMinDepth, 1, 5, 4);
ENGINE_PARAM(SeePruningDepth, 5, 12, 9);
ENGINE_PARAM(SeePruningQuietMargin, -110, -50, -80);
ENGINE_PARAM(SeePruningNoisyMargin, -50, -10, -15);
ENGINE_PARAM(HistBonus, 200, 400, 251);
ENGINE_PARAM(HistMax, 1500, 3500, 2425);
ENGINE_PARAM(CorrWeight, 10, 40, 35);
ENGINE_PARAM(LMRMinDepth, 2, 6, 3);
ENGINE_PARAM(AspStartWindow, 10, 30, 15);
ENGINE_PARAM(NodeTmFactor1, 100, 200, 108);
ENGINE_PARAM(NodeTmFactor2, 125, 225, 134);
ENGINE_PARAM(BmFactor1, 100, 200, 109);
ENGINE_PARAM(RazorMargin, 100, 500, 140);
ENGINE_PARAM(ProbCutMargin, 100, 500, 191);
ENGINE_PARAM(HistPruneDepth, 2, 8, 4);
ENGINE_PARAM(HistPruneThreshold, 800, 2000, 1200);
ENGINE_PARAM(HistExtThreshold, 3000, 15000, 7000);
ENGINE_PARAM(DeltaMarginBase, 50, 400, 180);
ENGINE_PARAM(HalfmoveScaleMax, 50, 500, 200);

#undef ENGINE_PARAM

inline void print_params_for_ob() {
  for (const Parameter &entry : params) {
    const double step = std::max(0.5, static_cast<double>(entry.max - entry.min) * 0.05);
    safe_printf("%s, int, %d, %d, %d, %f, 0.002\n", entry.name.c_str(),
                entry.value, entry.min, entry.max, step);
  }
}

inline void init_LMR() noexcept {
  const double offset = static_cast<double>(LMRBase) * 0.1;
  const double denom = static_cast<double>(LMRRatio) * 0.1;
  for (int d = 0; d <= MaxSearchDepth; ++d) {
    const double log_d = std::log1p(static_cast<double>(d));
    for (int m = 0; m < MaxActions; ++m) {
      const double log_m = std::log1p(static_cast<double>(m));
      const double reduction = offset + (log_d * log_m) / denom;
      LMRTable[d][m] = static_cast<int>(reduction + 0.5);
    }
  }
}
