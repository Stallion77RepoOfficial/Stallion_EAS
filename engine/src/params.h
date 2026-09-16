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

inline int MaterialBasis[6] = {0, 210, 800, 840, 1300, 2600};

constexpr inline int QuadraticImbalance[6][6] = {
    {0, 0, 0, 0, 0, 0},
    {0, -3, -1, 2, 7, 0},
    {0, -1, 5, 4, 5, 8},
    {0, 0, 3, 5, 4, 9},
    {0, -4, 2, 3, 4, 3},
    {0, 1, -3, -3, 0, 0}
};

inline int NormalizationFactor = 195;
inline int HALFMOVE_SCALE_MAX = 200;
inline int DELTA_MARGIN_BASE = 180;
inline int VARIETY_BASE_THRESHOLD = 150;
inline int VARIETY_MULTIPLIER = 2;
inline int PROMO_BONUS_DOUBLE_FORK = 250;
inline int PROMO_BONUS_SINGLE_FORK = 100;

inline int PawnStormConfig[4] = {110, 75, 40, 15};
inline int SpaceWeight = 7;

inline int Contempt = -15;
inline int TempoBonus = 16;

inline int TropismQueenWeight = 6;
inline int TropismRookWeight = 4;
inline int TropismKnightWeight = 4;
inline int TropismBishopWeight = 3;

inline int ThreatPawnAttack = 28;
inline int ThreatMinorOnHeavy = 40;
inline int ThreatRookOnQueen = 35;
inline int ThreatRookOnMinor = 15;
inline int ThreatHanging = 18;

inline int KSPawnShield = 20;
inline int KSPawnClose = 15;
inline int KSPawnMed = 10;
inline int KSNoPawn = -30;
inline int KSOpenFile = -25;
inline int KSSafeSqLow = -60;
inline int KSSafeSqMed = -20;
inline int KSCastleBonus = 10;
inline int KSCastledFlank = 30;
inline int KSCentralKingMajor = -50;
inline int KSCentralKingMinor = -25;
inline int KSAdvancedKing = -40;
inline int KSMovedKingCastle = -60;
inline int KSUncastledKing = -40;

inline int KZAttackWeight[7] = {0, 1, 3, 3, 4, 7, 0};
inline int KZDangerMultiplier = 5;
inline int KZMultiAttackerBonus = 2;
inline int KZSingleAttackerThreshold = 5;
inline int KZSingleAttackerPenalty = 3;
inline int KZNoQueenBonus = 30;
inline int KZBishopXray = 1;
inline int KZRookXray = 2;

inline int EGCenterDist = 10;
inline int EGKingDist = 5;
inline int EGMaterialThreshold = 2000;
inline int EGMaterialAdvantage = 200;

inline int BishopPairBonus = 50;
inline int RookOpenFile = 20;
inline int RookSemiOpenFile = 10;
inline int PassedPawnBase = 20;
inline int PassedPawnRankMul = 4;
inline int PassedPawnBlocked = -12;
inline int PassedPawnKingProximity = 25;
inline int PassedPawnKingProximityRank = 5;
inline int IsolatedPawnPenalty = -15;
inline int DoubledPawnPenalty = -10;
inline int OutpostBonus = 35;

inline int CenterKnight = 15;
inline int CenterBishop = 12;
inline int CenterPawn = 10;

inline int MobilityKnightBase = 4;
inline int MobilityBishopBase = 6;
inline int MobilityBishopMul = 3;
inline int MobilityBishopDiv = 4;
inline int MobilityRookBase = 7;
inline int MobilityRookMul = 2;
inline int MobilityRookDiv = 3;
inline int MobilityQueenBase = 14;
inline int MobilityQueenDiv = 3;
inline int MobilityEarlyQueenBonus = 8;

inline int UndevelopedPenalty = 5;

inline int EvalMultBase = 800;
inline int EvalMultMatDiv = 24;
inline int EvalMultNorm = 1024;
inline int EvalWinningMul = 120;
inline int EvalWinningMatThreshold = 4000;
inline int EvalSlightWinMul = 110;
inline int EvalSlightWinMatThreshold = 2500;
inline int EvalLosingMul = 90;
inline int EvalLosingThreshold = -150;
inline int EvalSlightLoseMul = 95;
inline int EvalSlightLoseThreshold = -50;

inline int SacPatternBonus = 55;
inline int SacKingFileBonus = 25;
inline int SacMultiBonus = 40;
inline int SacMaterialThreshold = 3000;

inline int DrawContemptMaterial = 60;
inline int HistExtThreshold = 7000;
inline int FPAttackModeBonus = 80;

inline int AttackModeEnterDepth = 6;
inline int AttackModeMaterial = 2800;
inline int AttackModeEnterRelax = 20;
inline int AttackModeExitRelax = 20;
inline int AttackModeDropExtra = 30;
inline int AttackModeMatExit = 200;

inline int PhaseConfirmHits = 2;
inline int SacrificeEnterCp = 250;
inline int SacrificeExitCp = 170;
inline int SacrificeDropThreshold = 120;
inline int LatePhaseMaterial = 4200;
inline int EndgameMaterial = 3000;
inline int MidRecoverMaterial = 4500;
inline int EndRecoverMaterial = 3300;
inline int OpeningMinPly = 20;

constexpr inline double WDL_A = -0.003;

constexpr inline int HUMAN_ELO_MIN = 500;
constexpr inline int HUMAN_ELO_RANGE = 1100;

inline void print_params_for_ob() {
  for (const auto &param : params) {
    printf("%s, int, %d, %d, %d, %f, 0.002\n", param.name.c_str(), param.value,
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
