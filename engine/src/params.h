#pragma once
#include "defs.h"
#include <cmath>
#include <iostream>
#include <vector>

MultiArray<int, 256 + 1, ListSize> LMRTable;


struct Parameter {
  std::string name;
  int &value;
  int min, max;
};

std::vector<Parameter> params;

// SPSA parameter code is based off Clover
// (https://github.com/lucametehau/CloverEngine)

struct CreateParam {
  int _value;
  CreateParam(std::string name, int value, int min, int max) : _value(value) {
    params.push_back({name, _value, min, max});
  }

  operator int() const { return _value; }
};


#define TUNE_PARAM(name, value, min, max)                                      \
  CreateParam name(#name, value, min, max);


TUNE_PARAM(NMPMinDepth, 3, 1, 5);
TUNE_PARAM(NMPBase, 3, 1, 5);
TUNE_PARAM(NMPDepthDiv, 4, 3, 9);
TUNE_PARAM(NMPEvalDiv, 100, 50, 300);
TUNE_PARAM(RFPMargin, 120, 50, 150);
TUNE_PARAM(RFPMaxDepth, 10, 6, 12);
TUNE_PARAM(LMRBase, 3, 1, 8);
TUNE_PARAM(LMRRatio, 25, 15, 30);
TUNE_PARAM(LMPBase, 2, 1, 5);
TUNE_PARAM(LMPDepth, 6, 3, 7);
TUNE_PARAM(SEDepth, 5, 4, 10);
TUNE_PARAM(SEDoubleExtMargin, 18, 10, 30);
TUNE_PARAM(FPDepth, 7, 5, 11);
TUNE_PARAM(FPMargin1, 150, 50, 200);
TUNE_PARAM(FPMargin2, 200, 75, 300);
TUNE_PARAM(IIRMinDepth, 2, 1, 5);
TUNE_PARAM(SeePruningDepth, 7, 5, 11);
TUNE_PARAM(SeePruningQuietMargin, -84, -110, -50);
TUNE_PARAM(SeePruningNoisyMargin, -35, -50, -10);
TUNE_PARAM(HistBonus, 282, 200, 400);
TUNE_PARAM(HistMax, 2565, 1500, 3500);
TUNE_PARAM(AgeDiffDiv, 5, 2, 6);
TUNE_PARAM(CorrWeight, 21, 10, 40);
TUNE_PARAM(LMRMinDepth, 3, 2, 6);
TUNE_PARAM(AspStartWindow, 20, 10, 30);
TUNE_PARAM(NodeTmFactor1, 149, 100, 200);
TUNE_PARAM(NodeTmFactor2, 177, 125, 225);
TUNE_PARAM(BmFactor1, 152, 100, 200);

// Material value constants (in centipawns)
const int KNIGHT_VALUE = 300;
const int BISHOP_VALUE = 300;
const int ROOK_VALUE = 500;
const int QUEEN_VALUE = 900;

// Halfmove draw scaling
const int HALFMOVE_SCALE_MAX = 200;

// Delta pruning in qsearch
const int DELTA_MARGIN_BASE = 200;

// Variety/multi-PV thresholds
const int VARIETY_BASE_THRESHOLD = 150; // at variety=0: 300cp
const int VARIETY_MULTIPLIER = 2;

// Promotion bonus in variety mode
const int PROMO_BONUS_DOUBLE_FORK = 200;
const int PROMO_BONUS_SINGLE_FORK = 75;

// Human mode ELO scaling
const int HUMAN_ELO_MIN = 500;
const int HUMAN_ELO_RANGE = 1100;

// Artık bu parametreler thread_info yapısında

void print_params_for_ob() {
  for (auto &param : params) {
    safe_printf("%s, int, %d, %d, %d, %f, 0.002\n",
                param.name.c_str(), param.value, param.min, param.max,
                std::max(0.5, (param.max - param.min) / 20.0));
  }
}

void init_LMR() {
  for (int i = 0; i < 256; i++) {
    for (int n = 0; n < ListSize; n++) {
      // CRITICAL FIX: Use log(1 + x) to avoid log(0) domain error
      // This ensures mathematical correctness for i=0 or n=0 cases
      double di = std::log(1.0 + static_cast<double>(i));
      double dn = std::log(1.0 + static_cast<double>(n));
      double val = (LMRBase / 10.0) + (di * dn) / (LMRRatio / 10.0);
      // Safety clamp to ensure non-negative reduction values
      if (val < 0.0) val = 0.0;
      LMRTable[i][n] = static_cast<int>(val + 0.5);
    }
  }
}
