#pragma once

#include "../runtime/include/ac/runtime.hpp"

namespace NativeBios {

struct PhotoDiscImage {
  unsigned width = 0;
  unsigned height = 0;
  std::string url;
  std::vector<std::uint32_t> pixels;
};

// Privileged Xbox/UWP boundary for removable photo media. This service owns
// every StorageFile and decoder object; sandboxed JavaScript sees only the
// bounded callbacks and immutable snapshot installed on Api::disc.
class PhotoDiscService final {
 public:
  using ImageReady = std::function<void(std::shared_ptr<const PhotoDiscImage>)>;
  using Logger = std::function<void(const std::string&)>;

  PhotoDiscService(ac::xbox::Api& api, ImageReady image_ready, Logger logger);

  void scan();
  void show(std::int64_t requested_index);
  void copy_all();

 private:
  concurrency::task<void> collect(
    Windows::Storage::StorageFolder^ folder,
    const std::shared_ptr<std::vector<Windows::Storage::StorageFile^>>& output,
    unsigned depth);
  concurrency::task<void> collect_optical_drive_letters(
    const std::shared_ptr<std::vector<Windows::Storage::StorageFile^>>& output,
    const std::shared_ptr<std::vector<std::string>>& volume_names);
  void update(const std::function<void(ac::xbox::PhotoDiscSnapshot&)>& edit);
  void fail_scan(const std::string& message);

  ac::xbox::Api& m_api;
  ImageReady m_imageReady;
  Logger m_log;
  std::mutex m_filesMutex;
  std::vector<Windows::Storage::StorageFile^> m_files;
  std::string m_volume;
  std::atomic_bool m_scanInFlight{false};
  std::atomic_bool m_copyInFlight{false};
  std::atomic_uint64_t m_loadGeneration{0};
};

}  // namespace NativeBios
