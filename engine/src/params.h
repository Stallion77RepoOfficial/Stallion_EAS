#pragma once
#include "defs.h"
#include <cmath>
#include <iostream>
#include <vector>

MultiArray<int, 256 + 1, MaxActions> LMRTable;

struct Parameter {
  std::string name;
  int &value;
  int min, max;
};

std::vector<Parameter> params;

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
TUNE_PARAM(SeePruningQuietMargin, -70, -110, -50);
TUNE_PARAM(SeePruningNoisyMargin, -25, -50, -10);
TUNE_PARAM(HistBonus, 282, 200, 400);
TUNE_PARAM(HistMax, 2565, 1500, 3500);

TUNE_PARAM(CorrWeight, 21, 10, 40);
TUNE_PARAM(LMRMinDepth, 3, 2, 6);
TUNE_PARAM(AspStartWindow, 20, 10, 30);
TUNE_PARAM(NodeTmFactor1, 149, 100, 200);
TUNE_PARAM(NodeTmFactor2, 177, 125, 225);
TUNE_PARAM(BmFactor1, 152, 100, 200);

// Piece values for material imbalance calculation
// PieceTypes: None=0, Pawn=1, Knight=2, Bishop=3, Rook=4, Queen=5
constexpr int MaterialBasis[6] = {0, 208, 781, 825, 1276, 2538};

// Quadratic Imbalance Table
// Matches Stockfish 13/14 era HCE imbalance values approx.
// Row: The piece we have. Col: The piece they have / other piece on board logic
constexpr int QuadraticImbalance[6][6] = {
    //  None   Pawn   Knight Bishop Rook   Queen
    {0, 0, 0, 0, 0, 0},   // None
    {0, -2, -2, 1, 6, 0}, // Pawn
    {0, -2, 4, 3, 4, 6},  // Knight
    {0, -1, 2, 3, 3, 7},  // Bishop
    {0, -3, 1, 2, 3, 2},  // Rook
    {0, 0, -4, -4, -1, 0} // Queen
};

const int HALFMOVE_SCALE_MAX = 200;

const int DELTA_MARGIN_BASE = 200;

const int VARIETY_BASE_THRESHOLD = 150;
const int VARIETY_MULTIPLIER = 2;

const int PROMO_BONUS_DOUBLE_FORK = 200;
const int PROMO_BONUS_SINGLE_FORK = 75;

// Pawn Storm Configuration
// Penalty for opponent pawns on files near our king (File of King-1 to King+1)
// Indexed by rank distance from our king (approx).
// 0: Very close (storm imminent), 1: Close, 2: Mid, 3: Far
constexpr int PawnStormConfig[4] = {95, 60, 30, 10};

// Space Evaluation Weight
// Bonus per "safe" square controlled in the center
constexpr int SpaceWeight = 5;

// WDL Time Management Constants
constexpr double WDL_A = -0.003; // Sigmoid slope
constexpr double WDL_B = 0.5;    // Center (not strictly used in simple 1/(1+e)
                                 // form but good for reference)

const int HUMAN_ELO_MIN = 500;
const int HUMAN_ELO_RANGE = 1100;

void print_params_for_ob() {
  for (auto &param : params) {
    printf("%s, int, %d, %d, %d, %f, 0.002\n", param.name.c_str(), param.value,
           param.min, param.max, std::max(0.5, (param.max - param.min) / 20.0));
  }
}

void init_LMR() {
  for (int i = 0; i < 256; i++) {
    for (int n = 0; n < MaxActions; n++) {

      double di = std::log(1.0 + static_cast<double>(i));
      double dn = std::log(1.0 + static_cast<double>(n));
      double val = (LMRBase / 10.0) + (di * dn) / (LMRRatio / 10.0);

      if (val < 0.0)
        val = 0.0;
      LMRTable[i][n] = static_cast<int>(val + 0.5);
    }
  }
}
