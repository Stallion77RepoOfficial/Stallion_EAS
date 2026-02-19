#pragma once
#include "defs.h"
#include <cmath>
#include <cstdio>
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

int MaterialBasis[6] = {0, 210, 800, 840, 1300, 2600};

int QuadraticImbalance[6][6] = {
    {0, 0, 0, 0, 0, 0},
    {0, -3, -1, 2, 7, 0},
    {0, -1, 5, 4, 5, 8},
    {0, 0, 3, 5, 4, 9},
    {0, -4, 2, 3, 4, 3},
    {0, 1, -3, -3, 0, 0}
};

int NormalizationFactor = 195;
int HALFMOVE_SCALE_MAX = 200;
int DELTA_MARGIN_BASE = 180;
int VARIETY_BASE_THRESHOLD = 150;
int VARIETY_MULTIPLIER = 2;
int PROMO_BONUS_DOUBLE_FORK = 250;
int PROMO_BONUS_SINGLE_FORK = 100;

int PawnStormConfig[4] = {110, 75, 40, 15};
int SpaceWeight = 7;

int Contempt = -15;
int TempoBonus = 16;

int TropismQueenWeight = 6;
int TropismRookWeight = 4;
int TropismKnightWeight = 4;
int TropismBishopWeight = 3;

int ThreatPawnAttack = 28;
int ThreatMinorOnHeavy = 40;
int ThreatRookOnQueen = 35;
int ThreatRookOnMinor = 15;
int ThreatHanging = 18;

int KSPawnShield = 20;
int KSPawnClose = 15;
int KSPawnMed = 10;
int KSNoPawn = -30;
int KSOpenFile = -25;
int KSSafeSqLow = -60;
int KSSafeSqMed = -20;
int KSCastleBonus = 10;
int KSCastledFlank = 30;
int KSCentralKingMajor = -50;
int KSCentralKingMinor = -25;
int KSAdvancedKing = -40;
int KSMovedKingCastle = -60;
int KSUncastledKing = -40;

int KZAttackWeight[7] = {0, 1, 3, 3, 4, 7, 0};
int KZDangerMultiplier = 5;
int KZMultiAttackerBonus = 2;
int KZSingleAttackerThreshold = 5;
int KZSingleAttackerPenalty = 3;
int KZNoQueenBonus = 30;
int KZBishopXray = 1;
int KZRookXray = 2;

int EGCenterDist = 10;
int EGKingDist = 5;
int EGPassedPawnRank = 2;
int EGMaterialThreshold = 2000;
int EGMaterialAdvantage = 200;

int BishopPairBonus = 50;
int RookOpenFile = 20;
int RookSemiOpenFile = 10;
int PassedPawnBase = 20;
int PassedPawnRankMul = 4;
int PassedPawnBlocked = -12;
int PassedPawnKingProximity = 25;
int PassedPawnKingProximityRank = 5;
int IsolatedPawnPenalty = -15;
int DoubledPawnPenalty = -10;
int OutpostBonus = 35;

int CenterKnight = 15;
int CenterBishop = 12;
int CenterPawn = 10;

int MobilityKnightBase = 4;
int MobilityBishopBase = 6;
int MobilityBishopMul = 3;
int MobilityBishopDiv = 4;
int MobilityRookBase = 7;
int MobilityRookMul = 2;
int MobilityRookDiv = 3;
int MobilityQueenBase = 14;
int MobilityQueenDiv = 3;
int MobilityEarlyQueenBonus = 8;

int UndevelopedPenalty = 5;

int EvalMultBase = 800;
int EvalMultMatDiv = 24;
int EvalMultNorm = 1024;
int EvalWinningMul = 120;
int EvalWinningMatThreshold = 4000;
int EvalSlightWinMul = 110;
int EvalSlightWinMatThreshold = 2500;
int EvalLosingMul = 90;
int EvalLosingThreshold = -150;
int EvalSlightLoseMul = 95;
int EvalSlightLoseThreshold = -50;

int SacPatternBonus = 55;
int SacKingFileBonus = 25;
int SacMultiBonus = 40;
int SacMaterialThreshold = 3000;

int DrawContemptMaterial = 60;
int HistExtThreshold = 7000;
int FPAttackModeBonus = 80;

int AttackModeEnterDepth = 6;
int AttackModeMaterial = 2800;
int AttackModeEnterRelax = 20;
int AttackModeExitRelax = 20;
int AttackModeDropExtra = 30;
int AttackModeMatExit = 200;
int AttackModeHistMul = 3;
int AttackModeHistDiv = 2;
int AttackModeHistAdd = 10;
int AttackModeHistCap = 256;

double WDL_A = -0.003;
double WDL_B = 0.5;

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
      LMRTable[i][n] = static_cast<int>(val + 0.5);
    }
  }
}
