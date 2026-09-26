
#include "search.h"
#include "uci.h"

#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <iostream>
#include <memory>
#include <optional>
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

std::vector<std::string_view> collect_args(int argc, char* argv[]) {
    if (argc <= 0 || argv == nullptr) return {};
    return std::vector<std::string_view>(argv, argv + argc);
}

std::optional<int> handle_cli_mode(const std::vector<std::string_view>& args,
                                   BoardState& position,
                                   ThreadInfo& thread_info) {
    if (args.size() <= 1) {
        return std::nullopt;
    }

    size_t total_len = 0;
    for (size_t i = 1; i < args.size(); ++i) {
        total_len += args[i].size() + 1;
    }

    std::string cmdline;
    cmdline.reserve(total_len);
    for (size_t i = 1; i < args.size(); ++i) {
        if (i > 1) cmdline += ' ';
        cmdline.append(args[i]);
    }

    std::istringstream cli_stream(cmdline);
    uci(thread_info, position, cli_stream, /*interactive=*/false);
    return 0;
}

} // namespace

int main(int argc, char* argv[]) {
    std::error_code ec;
    std::filesystem::path executable;
#if defined(__APPLE__)
    uint32_t size = 0;
    _NSGetExecutablePath(nullptr, &size);
    std::vector<char> path(size);
    if (_NSGetExecutablePath(path.data(), &size) == 0) executable = path.data();
#elif defined(_WIN32)
    std::vector<wchar_t> path(32768);
    const auto size = GetModuleFileNameW(nullptr, path.data(), static_cast<DWORD>(path.size()));
    if (size && size < path.size()) executable = std::wstring(path.data(), size);
#else
    executable = std::filesystem::read_symlink("/proc/self/exe", ec);
#endif
    if (executable.empty() && argc > 0 && argv != nullptr && argv[0] != nullptr) {
        executable = std::filesystem::absolute(argv[0], ec);
    }
    engine_directory = std::filesystem::weakly_canonical(executable, ec).parent_path();
    BoardState position{};
    auto thread_info = std::make_unique<ThreadInfo>();
    init_LMR();
    resize_TT(DefaultHashMB);

    const auto args = collect_args(argc, argv);
    if (const auto exit_code = handle_cli_mode(args, position, *thread_info)) {
        return *exit_code;
    }

    uci(*thread_info, position);
    return 0;
}
