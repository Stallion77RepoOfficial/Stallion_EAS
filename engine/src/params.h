#pragma once
#include "defs.h"
#include <algorithm>
#include <cmath>
#include <cstdio>
#include <vector>

inline MultiArray<int, 256 + 1, MaxActions> LMRTable{};

struct Parameter {
  std::string name;
  int &value;
  int min, max;
};

inline std::vector<Parameter> params;

struct CreateParam {
  int _value;
  CreateParam(std::string name, int value, int min, int max) : _value(value) {
    params.push_back({name, _value, min, max});
  }

  operator int() const { return _value; }
};

#define TUNE_PARAM(name, value, min, max)                                      \
  inline CreateParam name(#name, value, min, max);

TUNE_PARAM(NMPMinDepth, 1, 1, 5);
TUNE_PARAM(NMPBase, 5, 1, 5);
TUNE_PARAM(NMPDepthDiv, 9, 3, 9);
TUNE_PARAM(NMPEvalDiv, 209, 50, 300);
TUNE_PARAM(RFPMargin, 70, 50, 150);
TUNE_PARAM(RFPMaxDepth, 8, 6, 12);
TUNE_PARAM(LMRBase, 4, 1, 8);
TUNE_PARAM(LMRRatio, 16, 15, 30);
TUNE_PARAM(LMPBase, 4, 1, 5);
TUNE_PARAM(LMPDepth, 5, 3, 7);
TUNE_PARAM(SEDepth, 7, 4, 10);
TUNE_PARAM(SEDoubleExtMargin, 14, 10, 30);
TUNE_PARAM(FPDepth, 8, 5, 11);
TUNE_PARAM(FPMargin1, 190, 50, 200);
TUNE_PARAM(FPMargin2, 262, 75, 300);
TUNE_PARAM(IIRMinDepth, 4, 1, 5);
TUNE_PARAM(SeePruningDepth, 7, 5, 11);
TUNE_PARAM(SeePruningQuietMargin, -99, -110, -50);
TUNE_PARAM(SeePruningNoisyMargin, -14, -50, -10);
TUNE_PARAM(HistBonus, 251, 200, 400);
TUNE_PARAM(HistMax, 2425, 1500, 3500);
TUNE_PARAM(CorrWeight, 35, 10, 40);
TUNE_PARAM(LMRMinDepth, 5, 2, 6);
TUNE_PARAM(AspStartWindow, 15, 10, 30);
TUNE_PARAM(NodeTmFactor1, 102, 100, 200);
TUNE_PARAM(NodeTmFactor2, 134, 125, 225);
TUNE_PARAM(BmFactor1, 109, 100, 200);
TUNE_PARAM(RazorMargin, 140, 100, 500);
TUNE_PARAM(ProbCutMargin, 191, 100, 500);
TUNE_PARAM(MultiCutDepth, 4, 3, 10);
TUNE_PARAM(MultiCutMoves, 6, 2, 8);
TUNE_PARAM(MultiCutCuts, 3, 1, 5);
TUNE_PARAM(HistPruneDepth, 4, 2, 8);
TUNE_PARAM(HistPruneThreshold, 6196, 1000, 8000);
inline int NormalizationFactor = 195;
inline int HALFMOVE_SCALE_MAX = 200;
inline int DELTA_MARGIN_BASE = 180;
inline int HistExtThreshold = 7000;

inline void print_params_for_ob() {
  for (const auto &param : params) {
    safe_printf("%s, int, %d, %d, %d, %f, 0.002\n", param.name.c_str(), param.value,
                param.min, param.max, std::max(0.5, (param.max - param.min) / 20.0));
  }
}

inline void init_LMR() noexcept {
  const double base = static_cast<double>(LMRBase) / 10.0;
  const double ratio = static_cast<double>(LMRRatio) / 10.0;
  for (int i = 0; i <= 256; i++) {
    const double di = std::log(1.0 + static_cast<double>(i));
    for (int n = 0; n < MaxActions; n++) {
      const double dn = std::log(1.0 + static_cast<double>(n));
      const double val = base + (di * dn) / ratio;
      LMRTable[i][n] = static_cast<int>(val + 0.5);
    }
  }
}
