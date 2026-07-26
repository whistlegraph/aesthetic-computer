#include "pch.h"
#include "PhotoDiscService.hpp"

#include <cctype>

using namespace Platform;
using namespace Windows::Foundation;
using namespace Windows::Graphics::Imaging;
using namespace Windows::Storage;
using namespace Windows::Storage::Streams;
using namespace concurrency;

namespace NativeBios {
namespace {

constexpr std::size_t kMaxPhotoFiles = 4096;
constexpr unsigned kMaxFolderDepth = 32;
constexpr std::uint64_t kMaxEncodedBytes = 128ull * 1024 * 1024;
constexpr unsigned kMaxDecodedSide = 2048;

std::string utf8(String^ value) {
  if (!value || value->IsEmpty()) return {};
  const int size = WideCharToMultiByte(CP_UTF8, 0, value->Data(), value->Length(),
    nullptr, 0, nullptr, nullptr);
  std::string result(static_cast<std::size_t>(size), '\0');
  WideCharToMultiByte(CP_UTF8, 0, value->Data(), value->Length(), result.data(),
    size, nullptr, nullptr);
  return result;
}

bool is_photo_file(StorageFile^ file) {
  if (!file) return false;
  auto extension = utf8(file->FileType);
  std::transform(extension.begin(), extension.end(), extension.begin(),
    [](unsigned char value) { return static_cast<char>(std::tolower(value)); });
  return extension == ".jpg" || extension == ".jpeg" || extension == ".jpe" ||
    extension == ".png" || extension == ".tif" || extension == ".tiff" ||
    extension == ".pcd";
}

std::string clean_error(String^ value) {
  auto result = utf8(value);
  for (auto& character : result)
    if (character == '\r' || character == '\n') character = ' ';
  if (result.size() > 512) result.resize(512);
  return result;
}

}  // namespace

PhotoDiscService::PhotoDiscService(ac::xbox::Api& api, ImageReady image_ready,
    Logger logger)
    : m_api(api), m_imageReady(std::move(image_ready)), m_log(std::move(logger)) {
  m_api.disc.scan = [this]() { scan(); };
  m_api.disc.show = [this](std::int64_t index) { show(index); };
  m_api.disc.copy = [this]() { copy_all(); };
}

void PhotoDiscService::update(
    const std::function<void(ac::xbox::PhotoDiscSnapshot&)>& edit) {
  const auto current = std::atomic_load(&m_api.disc.snapshot);
  auto next = std::make_shared<ac::xbox::PhotoDiscSnapshot>();
  if (current) *next = *current;
  edit(*next);
  std::atomic_store(&m_api.disc.snapshot,
    std::static_pointer_cast<const ac::xbox::PhotoDiscSnapshot>(next));
}

task<void> PhotoDiscService::collect(StorageFolder^ folder,
    const std::shared_ptr<std::vector<StorageFile^>>& output, unsigned depth) {
  if (!folder || depth > kMaxFolderDepth || output->size() >= kMaxPhotoFiles)
    return task_from_result();
  return create_task(folder->GetFilesAsync()).then(
    [folder, output](IVectorView<StorageFile^>^ files) {
      for (auto file : files) {
        if (output->size() >= kMaxPhotoFiles) break;
        if (is_photo_file(file)) output->push_back(file);
      }
      return create_task(folder->GetFoldersAsync());
    }).then([this, output, depth](IVectorView<StorageFolder^>^ folders) {
      task<void> chain = task_from_result();
      for (auto child : folders) {
        chain = chain.then([this, child, output, depth]() {
          return collect(child, output, depth + 1);
        });
      }
      return chain;
    });
}

void PhotoDiscService::fail_scan(const std::string& message) {
  update([&message](ac::xbox::PhotoDiscSnapshot& snapshot) {
    snapshot.status = "error: " + message;
    snapshot.count = 0;
    snapshot.current_ready = false;
  });
  if (m_log) m_log("AC_NATIVE_DISC_ERROR " + message);
}

void PhotoDiscService::scan() {
  bool expected = false;
  if (!m_scanInFlight.compare_exchange_strong(expected, true)) return;
  ++m_loadGeneration;
  {
    std::lock_guard<std::mutex> lock(m_filesMutex);
    m_files.clear();
    m_volume.clear();
  }
  update([](ac::xbox::PhotoDiscSnapshot& snapshot) {
    snapshot = {};
    snapshot.status = "scanning";
  });
  if (m_log) m_log("AC_NATIVE_DISC_SCAN begin=1");

  auto photos = std::make_shared<std::vector<StorageFile^>>();
  auto volumeNames = std::make_shared<std::vector<std::string>>();
  create_task(KnownFolders::RemovableDevices->GetFoldersAsync()).then(
    [this, photos, volumeNames](IVectorView<StorageFolder^>^ volumes) {
      if (!volumes || volumes->Size == 0)
        throw std::runtime_error("no mounted removable volume");
      task<void> chain = task_from_result();
      for (auto volume : volumes) {
        volumeNames->push_back(utf8(volume->Name));
        chain = chain.then([this, volume, photos]() {
          return collect(volume, photos, 0);
        });
      }
      return chain;
    }).then([this, photos, volumeNames](task<void> completed) {
      try {
        completed.get();
        std::sort(photos->begin(), photos->end(), [](StorageFile^ left, StorageFile^ right) {
          return utf8(left ? left->Path : nullptr) < utf8(right ? right->Path : nullptr);
        });
        std::string volume;
        for (const auto& name : *volumeNames) {
          if (!volume.empty()) volume += ", ";
          volume += name;
        }
        {
          std::lock_guard<std::mutex> lock(m_filesMutex);
          m_files = *photos;
          m_volume = volume;
        }
        update([photos, &volume](ac::xbox::PhotoDiscSnapshot& snapshot) {
          snapshot.status = photos->empty() ? "empty" : "ready";
          snapshot.volume = volume;
          snapshot.count = photos->size();
          snapshot.index = 0;
          snapshot.current_ready = false;
        });
        if (m_log) m_log("AC_NATIVE_DISC_READY volumes=" +
          std::to_string(volumeNames->size()) + " photos=" +
          std::to_string(photos->size()) + " formats=jpg,jpeg,jpe,png,tif,tiff,pcd");
        m_scanInFlight = false;
        if (!photos->empty()) show(0);
        return;
      } catch (Exception^ error) {
        fail_scan(clean_error(error->Message));
      } catch (const std::exception& error) {
        fail_scan(error.what());
      }
      m_scanInFlight = false;
    });
}

void PhotoDiscService::show(std::int64_t requested_index) {
  StorageFile^ file = nullptr;
  std::size_t index = 0;
  std::size_t count = 0;
  {
    std::lock_guard<std::mutex> lock(m_filesMutex);
    count = m_files.size();
    if (count == 0) return;
    const auto modulus = static_cast<std::int64_t>(count);
    index = static_cast<std::size_t>((requested_index % modulus + modulus) % modulus);
    file = m_files[index];
  }
  if (!file) return;
  const auto generation = ++m_loadGeneration;
  const auto name = utf8(file->Name);
  update([index, count, &name](ac::xbox::PhotoDiscSnapshot& snapshot) {
    snapshot.status = "loading";
    snapshot.index = index;
    snapshot.count = count;
    snapshot.name = name;
    snapshot.width = 0;
    snapshot.height = 0;
    snapshot.current_ready = false;
  });
  if (m_log) m_log("AC_NATIVE_DISC_LOAD index=" + std::to_string(index) +
    " name=" + name);

  create_task(file->OpenAsync(FileAccessMode::Read)).then(
    [](IRandomAccessStreamWithContentType^ stream) {
      if (!stream || stream->Size == 0 || stream->Size > kMaxEncodedBytes)
        throw std::runtime_error("photo payload is empty or exceeds 128 MiB");
      return create_task(BitmapDecoder::CreateAsync(stream)).then(
        [stream](BitmapDecoder^ decoder) {
          if (!decoder || decoder->PixelWidth == 0 || decoder->PixelHeight == 0)
            throw std::runtime_error("image decoder returned an empty frame");
          const double scale = (std::min)(1.0, kMaxDecodedSide /
            static_cast<double>((std::max)(decoder->PixelWidth, decoder->PixelHeight)));
          const unsigned width = (std::max)(1u,
            static_cast<unsigned>(decoder->PixelWidth * scale));
          const unsigned height = (std::max)(1u,
            static_cast<unsigned>(decoder->PixelHeight * scale));
          auto transform = ref new BitmapTransform();
          transform->ScaledWidth = width;
          transform->ScaledHeight = height;
          return create_task(decoder->GetPixelDataAsync(BitmapPixelFormat::Bgra8,
            BitmapAlphaMode::Straight, transform, ExifOrientationMode::RespectExifOrientation,
            ColorManagementMode::ColorManageToSRgb)).then(
              [stream, width, height](PixelDataProvider^ provider) {
                const auto bytes = provider->DetachPixelData();
                const auto pixelCount = static_cast<std::size_t>(width) * height;
                if (!bytes || bytes->Length < pixelCount * 4)
                  throw std::runtime_error("image decoder returned a short pixel buffer");
                auto image = std::make_shared<PhotoDiscImage>();
                image->width = width;
                image->height = height;
                image->pixels.resize(pixelCount);
                for (std::size_t i = 0; i < pixelCount; ++i) {
                  const auto offset = i * 4;
                  image->pixels[i] = (static_cast<std::uint32_t>(bytes[offset + 3]) << 24) |
                    (static_cast<std::uint32_t>(bytes[offset + 2]) << 16) |
                    (static_cast<std::uint32_t>(bytes[offset + 1]) << 8) |
                    static_cast<std::uint32_t>(bytes[offset]);
                }
                return image;
              });
        });
    }).then([this, generation, index, count, name](
        task<std::shared_ptr<PhotoDiscImage>> completed) {
      if (generation != m_loadGeneration.load()) return;
      try {
        auto image = completed.get();
        if (m_imageReady)
          m_imageReady(std::static_pointer_cast<const PhotoDiscImage>(image));
        update([index, count, &name, &image](ac::xbox::PhotoDiscSnapshot& snapshot) {
          snapshot.status = "ready";
          snapshot.index = index;
          snapshot.count = count;
          snapshot.name = name;
          snapshot.width = image->width;
          snapshot.height = image->height;
          snapshot.current_ready = true;
        });
        if (m_log) m_log("AC_NATIVE_DISC_IMAGE_READY index=" +
          std::to_string(index) + " size=" + std::to_string(image->width) + "x" +
          std::to_string(image->height) + " name=" + name);
      } catch (Exception^ error) {
        const auto message = clean_error(error->Message);
        update([&message](ac::xbox::PhotoDiscSnapshot& snapshot) {
          snapshot.status = "decode-error: " + message;
          snapshot.current_ready = false;
        });
        if (m_log) m_log("AC_NATIVE_DISC_DECODE_ERROR " + message);
      } catch (const std::exception& error) {
        const std::string message(error.what());
        update([&message](ac::xbox::PhotoDiscSnapshot& snapshot) {
          snapshot.status = "decode-error: " + message;
          snapshot.current_ready = false;
        });
        if (m_log) m_log("AC_NATIVE_DISC_DECODE_ERROR " + message);
      }
    });
}

void PhotoDiscService::copy_all() {
  bool expected = false;
  if (!m_copyInFlight.compare_exchange_strong(expected, true)) return;
  auto files = std::make_shared<std::vector<StorageFile^>>();
  {
    std::lock_guard<std::mutex> lock(m_filesMutex);
    *files = m_files;
  }
  if (files->empty()) {
    update([](ac::xbox::PhotoDiscSnapshot& snapshot) {
      snapshot.copy_status = "no-photos";
    });
    m_copyInFlight = false;
    return;
  }
  auto copied = std::make_shared<std::atomic_size_t>(0);
  auto failed = std::make_shared<std::atomic_size_t>(0);
  update([](ac::xbox::PhotoDiscSnapshot& snapshot) {
    snapshot.copy_status = "copying";
    snapshot.copied = 0;
    snapshot.copy_failed = 0;
  });
  create_task(ApplicationData::Current->LocalFolder->CreateFolderAsync(
    L"photo-cd", CreationCollisionOption::OpenIfExists)).then(
      [this, files, copied, failed](StorageFolder^ destination) {
        task<void> chain = task_from_result();
        for (std::size_t index = 0; index < files->size(); ++index) {
          auto file = (*files)[index];
          chain = chain.then([this, destination, file, index, copied, failed]() {
            std::wstring name = file && file->Name ? file->Name->Data() : L"photo";
            if (name.size() > 220) name.resize(220);
            wchar_t prefix[24]{};
            swprintf_s(prefix, L"%05llu-", static_cast<unsigned long long>(index + 1));
            name.insert(0, prefix);
            return create_task(file->CopyAsync(destination, ref new String(name.c_str()),
              NameCollisionOption::ReplaceExisting)).then(
                [this, copied, failed](task<StorageFile^> completed) {
                  try { completed.get(); ++(*copied); }
                  catch (...) { ++(*failed); }
                  update([copied, failed](ac::xbox::PhotoDiscSnapshot& snapshot) {
                    snapshot.copied = copied->load();
                    snapshot.copy_failed = failed->load();
                  });
                });
          });
        }
        return chain;
      }).then([this, files, copied, failed](task<void> completed) {
        std::string error;
        try { completed.get(); }
        catch (Exception^ value) { error = clean_error(value->Message); }
        catch (const std::exception& value) { error = value.what(); }
        update([&error, copied, failed](ac::xbox::PhotoDiscSnapshot& snapshot) {
          snapshot.copy_status = error.empty() ? "complete" : "error: " + error;
          snapshot.copied = copied->load();
          snapshot.copy_failed = failed->load();
        });
        if (m_log) m_log("AC_NATIVE_DISC_COPY copied=" +
          std::to_string(copied->load()) + " failed=" +
          std::to_string(failed->load()) + " total=" +
          std::to_string(files->size()) + (error.empty() ? "" : " error=" + error));
        m_copyInFlight = false;
      });
}

}  // namespace NativeBios
