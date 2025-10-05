#!/usr/bin/env python3

import argparse
import subprocess
import multiprocessing as mp
import random
import chess
from pathlib import Path
from tqdm import tqdm

def write_positions_to_file(output_path, lock, positions):
    with lock:
        with open(output_path, 'a') as f:
            for pos in positions:
                f.write(pos + '\n')

def generate_game(args):
    game_id, engine_path, depth, output_path, lock = args
    
    board = chess.Board()
    positions = []
    
    # Variable opening length (8-16 moves) for diversity
    opening_moves = random.randint(8, 16)
    for _ in range(opening_moves):
        if board.is_game_over():
            break
        moves = list(board.legal_moves)
        if moves:
            board.push(random.choice(moves))
    
    move_count = 0
    last_score = 0
    adjudication_count = 0
    
    while not board.is_game_over() and move_count < 400:
        try:
            # Random move chance (5%) to increase diversity
            if random.random() < 0.05:
                moves = list(board.legal_moves)
                if moves:
                    bestmove = random.choice(moves)
                    score = 0
                else:
                    break
            else:
                result = subprocess.run(
                    [engine_path, "uci"],
                    input=f"position fen {board.fen()}\ngo depth {depth}\nquit\n",
                    capture_output=True,
                    text=True,
                    timeout=30
                )
                
                bestmove = None
                score = 0
                
                for line in result.stdout.split('\n'):
                    if 'score cp' in line:
                        parts = line.split()
                        try:
                            idx = parts.index('cp')
                            score = int(parts[idx + 1])
                        except:
                            pass
                            
                    if line.startswith('bestmove'):
                        try:
                            bestmove = chess.Move.from_uci(line.split()[1])
                        except:
                            pass
                
                if not bestmove or bestmove not in board.legal_moves:
                    break
            
            if move_count >= 10:
                positions.append({
                    'fen': board.fen(),
                    'score': score,
                    'move_count': move_count
                })
            
            if abs(score) > 800:
                adjudication_count += 1
                if adjudication_count >= 3:
                    result_val = 1.0 if score > 0 else 0.0
                    break
            else:
                adjudication_count = 0
            
            board.push(bestmove)
            move_count += 1
            last_score = score
            
        except (subprocess.TimeoutExpired, Exception):
            break
    
    if board.is_game_over():
        result_str = board.result()
        if result_str == "1-0":
            result_val = 1.0
        elif result_str == "0-1":
            result_val = 0.0
        else:
            result_val = 0.5
    else:
        result_val = 1.0 if last_score > 0 else 0.0 if last_score < 0 else 0.5
    
    output = []
    for pos in positions:
        output.append(f"{pos['fen']}|{pos['score']}|{result_val}")
    
    write_positions_to_file(output_path, lock, output)
    
    return len(output)

def main():
    parser = argparse.ArgumentParser(description="Stallion Self-Play Data Generation")
    parser.add_argument('--games', type=int, default=10000, help='Number of games')
    parser.add_argument('--threads', type=int, default=8, help='Number of threads')
    parser.add_argument('--depth', type=int, default=10, help='Search depth')
    parser.add_argument('--output', type=str, default='data.txt', help='Output file')
    parser.add_argument('--engine', type=str, default='../engine/stallion_eas_mac', help='Engine path')
    parser.add_argument('--random-chance', type=float, default=0.05, help='Random move probability (0.0-1.0)')
    parser.add_argument('--opening-moves', type=str, default='8-16', help='Opening moves range (min-max)')
    
    args = parser.parse_args()
    
    engine_path = Path(args.engine).resolve()
    if not engine_path.exists():
        print(f"Error: Engine not found: {engine_path}")
        return 1
    
    print("=" * 60)
    print("Stallion Self-Play Data Generation")
    print("=" * 60)
    print(f"Engine:  {engine_path}")
    print(f"Games:   {args.games:,}")
    print(f"Threads: {args.threads}")
    print(f"Depth:   {args.depth}")
    print(f"Output:  {args.output}")
    print("=" * 60)
    print()
    
    output_path = Path(args.output)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    
    if output_path.exists():
        output_path.unlink()
    
    manager = mp.Manager()
    lock = manager.Lock()
    
    worker_args = [
        (i, str(engine_path), args.depth, str(output_path), lock)
        for i in range(args.games)
    ]
    
    print(f"Starting generation with {args.threads} threads...")
    print("Writing positions to file as games complete...")
    print()
    
    total_positions = 0
    
    with mp.Pool(args.threads) as pool:
        for position_count in tqdm(
            pool.imap_unordered(generate_game, worker_args),
            total=args.games,
            desc="Games"
        ):
            total_positions += position_count
    
    print()
    print("=" * 60)
    print("Completed!")
    print("=" * 60)
    print(f"Total positions: {total_positions:,}")
    print(f"Positions/game:  {total_positions/args.games:.1f}")
    print(f"File:            {output_path} ({output_path.stat().st_size/1024/1024:.1f} MB)")
    print("=" * 60)
    print()
    
    return 0

if __name__ == "__main__":
    import sys
    sys.exit(main())
