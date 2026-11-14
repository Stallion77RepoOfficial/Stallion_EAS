#include "selfplay.h"
#include "uci.h"

#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <memory>
#include <sstream>
#include <string>
#include <string_view>
#include <system_error>
#include <vector>

#if defined(__APPLE__)
#include <mach-o/dyld.h>
#elif defined(_WIN32)
#include <windows.h>
#endif

namespace {

std::filesystem::path locate_binary_dir(int argc, char *argv[]) {
  std::error_code ec;
  std::filesystem::path binary_path;
#if defined(__APPLE__)
  uint32_t buf_len = 0;
  _NSGetExecutablePath(nullptr, &buf_len);
  std::string raw_path(buf_len, '\0');
  if (_NSGetExecutablePath(raw_path.data(), &buf_len) == 0) {
    binary_path = raw_path.c_str();
  }
#elif defined(_WIN32)
  std::vector<wchar_t> raw_path(32768);
  const DWORD copied =
      GetModuleFileNameW(nullptr, raw_path.data(), static_cast<DWORD>(raw_path.size()));
  if (copied > 0 && copied < raw_path.size()) {
    binary_path = std::wstring(raw_path.data(), copied);
  }
#else
  binary_path = std::filesystem::read_symlink("/proc/self/exe", ec);
#endif
  if (binary_path.empty() && argc > 0 && argv != nullptr && argv[0] != nullptr) {
    binary_path = std::filesystem::absolute(argv[0], ec);
  }
  return std::filesystem::weakly_canonical(binary_path, ec).parent_path();
}

int run_non_interactive(const std::vector<std::string_view> &args,
                        BoardState &pos, ThreadInfo &ti) {
  if (args[1] == "selfplay") return native_selfplay(args, ti);

  std::string one_shot;
  for (size_t idx = 1; idx < args.size(); ++idx) {
    if (idx > 1) one_shot.push_back(' ');
    one_shot.append(args[idx]);
  }
  std::istringstream single_input(std::move(one_shot));
  uci(ti, pos, single_input, /*interactive=*/false);
  return 0;
}

}  // namespace

int main(int argc, char *argv[]) {
  engine_directory = locate_binary_dir(argc, argv);

  init_LMR();
  resize_TT(DefaultHashMB);

  BoardState board{};
  auto main_thread = std::make_unique<ThreadInfo>();

  if (argc > 1 && argv != nullptr) {
    std::vector<std::string_view> cli_args(argv, argv + argc);
    return run_non_interactive(cli_args, board, *main_thread);
  }

  uci(*main_thread, board);
  return 0;
}
