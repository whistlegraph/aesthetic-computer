#pragma once
#include "../runtime/include/ac/runtime.hpp"
#include <fstream>
#include <cmath>
#include <memory>
#include <algorithm>
#include <mutex>

namespace NativeBios {
// Browser-only pairing: the public QR never contains the polling credential,
// and the game never receives the account token. All URLs are fixed here.
class OskiewarAccountService final : public std::enable_shared_from_this<OskiewarAccountService> {
 public:
  explicit OskiewarAccountService(ac::xbox::Api& api) {
    m_path = std::wstring(Windows::Storage::ApplicationData::Current->LocalFolder->Path->Data()) +
      L"\\oskiewar-account.json";
    try {
      std::ifstream input(m_path.c_str(), std::ios::binary);
      std::string saved((std::istreambuf_iterator<char>(input)), {});
      if (!saved.empty() && saved.size() < 65536) {
        auto json = Windows::Data::Json::JsonObject::Parse(wide(saved));
        const auto handle = utf8(json->GetNamedString(L"handle", L""));
        auto session = json->GetNamedObject(L"session", nullptr);
        if (valid_handle(handle) && session && !session->GetNamedString(L"accessToken", L"")->IsEmpty()) {
          m_handle = handle; m_token = utf8(session->GetNamedString(L"accessToken", L"")); m_status = "signed-in";
        }
      }
    } catch (...) {}
    api.account_state = [this]() { return state(); };
    api.account_action = [this](std::string_view action, std::string_view payload) { act(action, payload); };
    api.account_report = [this](std::string_view payload) { return report(payload); };
  }

  void act(std::string_view action, std::string_view payload = {}) {
    if (action == "leaderboard") { leaderboard(payload); return; }
    if (action != "login" && action != "logout" && action != "cancel") return;
    unsigned generation;
    {
      std::lock_guard<std::mutex> lock(m_mutex);
      ++m_generation; generation = m_generation;
      m_busy = false; m_reportBusy = m_leaderboardBusy = false; m_secret.clear(); m_code.clear(); m_error.clear(); m_expires = 0;
      if (action == "logout") {
        // The game's own private account file only. OS/developer credentials
        // and the phone's login session are deliberately outside this scope.
        std::ofstream clear(m_path.c_str(), std::ios::binary | std::ios::trunc);
        if (!clear) { m_status = "error"; m_error = "Could not clear sign-in"; return; }
        clear << "{}"; clear.close(); m_handle.clear(); m_token.clear(); m_pendingReport.clear(); m_reportStatus.clear(); m_status = "signed-out";
        return;
      }
      if (action == "cancel") { m_status = m_handle.empty() ? "signed-out" : "signed-in"; return; }
      if (!m_handle.empty()) { m_status = "signed-in"; return; }
      m_status = "creating"; m_busy = true;
    }
    request(generation, true, {});
  }

  std::string state() {
    unsigned generation = 0;
    std::string poll, queuedReport;
    {
      std::lock_guard<std::mutex> lock(m_mutex);
      const auto now = GetTickCount64();
      if (!m_reportBusy && !m_pendingReport.empty() && now >= m_nextReport)
        queuedReport = m_pendingReport;
      if (m_status == "waiting" && now >= m_deadline) {
        ++m_generation; m_busy = false; m_secret.clear(); m_code.clear();
        m_status = "error"; m_error = "Code expired. Sign in again."; m_expires = 0;
      } else if (m_status == "waiting" && !m_busy && now >= m_nextPoll) {
        m_busy = true; generation = m_generation;
        poll = "https://aesthetic.computer/api/device-pair?code=" + m_code + "&secret=" + m_secret;
      }
    }
    if (!poll.empty()) request(generation, false, poll);
    if (!queuedReport.empty()) report(queuedReport);
    std::lock_guard<std::mutex> lock(m_mutex);
    auto json = ref new Windows::Data::Json::JsonObject();
    using Windows::Data::Json::JsonValue;
    json->Insert(L"status", JsonValue::CreateStringValue(wide(m_status)));
    json->Insert(L"handle", JsonValue::CreateStringValue(wide(m_handle)));
    json->Insert(L"code", JsonValue::CreateStringValue(wide(m_code)));
    json->Insert(L"pairUrl", JsonValue::CreateStringValue(wide(m_code.empty() ? "" :
      "https://aesthetic.computer/api/device-pair-login?code=" + m_code)));
    json->Insert(L"expiresAt", JsonValue::CreateNumberValue(static_cast<double>(m_expires)));
    json->Insert(L"error", JsonValue::CreateStringValue(wide(m_error)));
    json->Insert(L"reportStatus", JsonValue::CreateStringValue(wide(m_reportStatus)));
    json->Insert(L"leaderboardError", JsonValue::CreateStringValue(wide(m_leaderboardError)));
    try { json->Insert(L"leaderboard", Windows::Data::Json::JsonObject::Parse(wide(m_leaderboard))); } catch (...) {}
    return utf8(json->Stringify());
  }

 private:
  static Platform::String^ wide(const std::string& text) {
    const int size = MultiByteToWideChar(CP_UTF8, 0, text.data(), static_cast<int>(text.size()), nullptr, 0);
    std::wstring result(size, L'\0');
    if (size) MultiByteToWideChar(CP_UTF8, 0, text.data(), static_cast<int>(text.size()), &result[0], size);
    return ref new Platform::String(result.c_str());
  }
  static std::string utf8(Platform::String^ text) {
    if (!text) return {};
    const int size = WideCharToMultiByte(CP_UTF8, 0, text->Data(), text->Length(), nullptr, 0, nullptr, nullptr);
    std::string result(size, '\0');
    if (size) WideCharToMultiByte(CP_UTF8, 0, text->Data(), text->Length(), &result[0], size, nullptr, nullptr);
    return result;
  }
  static bool valid_handle(const std::string& value) {
    return !value.empty() && value.size() <= 64 && value.find_first_not_of(
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._-") == std::string::npos;
  }
  static std::int64_t unix_ms() {
    FILETIME ft; GetSystemTimeAsFileTime(&ft);
    ULARGE_INTEGER n; n.LowPart = ft.dwLowDateTime; n.HighPart = ft.dwHighDateTime;
    return static_cast<std::int64_t>(n.QuadPart / 10000ULL - 11644473600000ULL);
  }
  void fail(unsigned generation, const char* message) {
    std::lock_guard<std::mutex> lock(m_mutex);
    if (generation != m_generation) return;
    m_busy = false;
    if (m_status == "waiting") {
      m_nextPoll = GetTickCount64() + 3500;
      m_error = "Reconnecting...";
    } else { m_status = "error"; m_error = message; }
  }
  void request(unsigned generation, bool creating, const std::string& poll) {
    using namespace Windows::Web::Http;
    using namespace Windows::Data::Json;
    using namespace concurrency;
    const auto lifetime = shared_from_this();
    auto client = ref new HttpClient();
    const auto url = creating ? "https://aesthetic.computer/api/device-pair" : poll;
    auto uri = ref new Windows::Foundation::Uri(wide(url));
    auto content = ref new HttpStringContent(L"{\"action\":\"create\",\"kind\":\"browser\"}",
      Windows::Storage::Streams::UnicodeEncoding::Utf8, L"application/json");
    auto pending = creating ? client->PostAsync(uri, content) : client->GetAsync(uri);
    create_task(pending).then([this, lifetime, generation, creating, client, content](HttpResponseMessage^ response) {
      if (!response->IsSuccessStatusCode) throw ref new Platform::FailureException();
      return response->Content->ReadAsStringAsync();
    }).then([this, lifetime, generation, creating, client](task<Platform::String^> completed) {
      try {
        auto body = completed.get();
        if (body->Length() > 65536) throw ref new Platform::FailureException();
        auto json = JsonObject::Parse(body);
        std::lock_guard<std::mutex> lock(m_mutex);
        if (generation != m_generation) return;
        m_busy = false; m_error.clear();
        if (creating) {
          const auto code = utf8(json->GetNamedString(L"code", L""));
          const auto secret = utf8(json->GetNamedString(L"pollSecret", L""));
          if (code.size() != 6 || code.find_first_not_of("ABCDEFGHJKLMNPQRSTUVWXYZ23456789") != std::string::npos ||
              secret.size() < 32 || secret.size() > 128 || secret.find_first_not_of(
                "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_") != std::string::npos) {
            m_status = "error"; m_error = "Could not create sign-in"; return;
          }
          m_code = code; m_secret = secret; m_status = "waiting";
          m_deadline = GetTickCount64() + 600000; m_expires = unix_ms() + 600000;
          m_nextPoll = GetTickCount64() + 1500;
        } else if (json->GetNamedString(L"status", L"") == L"claimed") {
          auto session = json->GetNamedObject(L"session", nullptr);
          const auto handle = utf8(json->GetNamedString(L"handle", L""));
          if (!valid_handle(handle) || !session || session->GetNamedString(L"accessToken", L"")->IsEmpty()) {
            m_status = "error"; m_error = "Sign-in response was incomplete"; return;
          }
          auto saved = ref new JsonObject();
          saved->Insert(L"handle", JsonValue::CreateStringValue(wide(handle)));
          saved->Insert(L"session", session);
          std::ofstream output(m_path.c_str(), std::ios::binary | std::ios::trunc);
          if (!output) { m_status = "error"; m_error = "Could not save sign-in"; return; }
          output << utf8(saved->Stringify()); output.close();
          if (!output) { m_status = "error"; m_error = "Could not save sign-in"; return; }
          m_handle = handle; m_token = utf8(session->GetNamedString(L"accessToken", L"")); m_status = "signed-in";
          m_secret.clear(); m_code.clear(); m_expires = 0;
        } else { m_nextPoll = GetTickCount64() + 2500; }
      } catch (...) { fail(generation, "Sign-in unavailable. Try again."); }
    });
  }
  void leaderboard(std::string_view payload) {
    using namespace Windows::Data::Json;
    std::string query;
    try {
      if (payload.size() > 256) return;
      auto handles = JsonArray::Parse(wide(std::string(payload)));
      if (handles->Size > 2) return;
      for (unsigned i=0; i<handles->Size; ++i) {
        const auto handle = utf8(handles->GetStringAt(i));
        if(handle.empty())continue;
        if (!valid_handle(handle)) return;
        if(query==handle)continue;
        query += (query.empty() ? "" : ",") + handle;
      }
    } catch (...) { return; }
    unsigned generation;
    {
      std::lock_guard<std::mutex> lock(m_mutex);
      const auto now=GetTickCount64();
      if (m_leaderboardBusy || (m_lastLeaderboard && now-m_lastLeaderboard<30000)) return;
      m_lastLeaderboard=now; m_leaderboardBusy=true; generation=m_generation;
    }
    const auto lifetime=shared_from_this();
    auto client=ref new Windows::Web::Http::HttpClient();
    auto uri=ref new Windows::Foundation::Uri(wide("https://aesthetic.computer/api/oskiewar-leaderboard?handles="+query));
    concurrency::create_task(client->GetAsync(uri)).then([client,lifetime](Windows::Web::Http::HttpResponseMessage^ response) {
      if (!response->IsSuccessStatusCode) throw ref new Platform::FailureException();
      return response->Content->ReadAsStringAsync();
    }).then([this,lifetime,generation](concurrency::task<Platform::String^> result) {
      try {
        const auto body=result.get();
        if (body->Length()>65536) throw ref new Platform::FailureException();
        auto json=JsonObject::Parse(body);
        std::lock_guard<std::mutex> lock(m_mutex);
        if (generation!=m_generation) return;
        m_leaderboardBusy=false; m_leaderboardError.clear();m_leaderboard=utf8(json->Stringify());
      } catch (...) {
        std::lock_guard<std::mutex> lock(m_mutex);
        if (generation!=m_generation) return;
        m_leaderboardBusy=false;m_leaderboardError="Leaderboard unavailable";
      }
    });
  }
  bool report(std::string_view payload) {
    using namespace Windows::Data::Json;
    std::string body, token; unsigned generation;
    try {
      if (payload.size()>4096) return false;
      auto request=JsonObject::Parse(wide(std::string(payload)));
      const auto match=utf8(request->GetNamedString(L"matchId",L""));
      const auto seat=request->GetNamedNumber(L"seat",-1), winner=request->GetNamedNumber(L"winner",-1);
      auto handles=request->GetNamedArray(L"handles",nullptr), wins=request->GetNamedArray(L"roundWins",nullptr);
      if (match.empty() || match.size()>128 || match.find_first_not_of(
          "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._:-")!=std::string::npos ||
          (seat!=0 && seat!=1) || (winner!=0 && winner!=1) || !handles || !wins || handles->Size!=2 || wins->Size!=2) return false;
      for(unsigned i=0;i<2;++i) {
        if(!valid_handle(utf8(handles->GetStringAt(i)))) return false;
        const double n=wins->GetNumberAt(i); if(!std::isfinite(n)||n<0||n>100||std::floor(n)!=n) return false;
      }
      auto clean=ref new JsonObject();
      clean->Insert(L"matchId",JsonValue::CreateStringValue(wide(match)));
      clean->Insert(L"seat",JsonValue::CreateNumberValue(seat));
      clean->Insert(L"winner",JsonValue::CreateNumberValue(winner));
      clean->Insert(L"handles",handles);clean->Insert(L"roundWins",wins);
      std::lock_guard<std::mutex> lock(m_mutex);
      if (m_reportBusy) return m_pendingReport == utf8(clean->Stringify());
      if(m_token.empty()||utf8(handles->GetStringAt(static_cast<unsigned>(seat)))!=m_handle) {
        m_reportStatus="Sign in to submit results";return false;
      }
      body=utf8(clean->Stringify());token=m_token;generation=m_generation;
      if(m_pendingReport!=body)m_reportRetries=0;
      m_pendingReport=body;m_reportBusy=true;m_reportStatus="Submitting result";
    } catch (...) {return false;}
    const auto lifetime=shared_from_this();
    auto client=ref new Windows::Web::Http::HttpClient();
    client->DefaultRequestHeaders->Authorization=ref new Windows::Web::Http::Headers::HttpCredentialsHeaderValue(L"Bearer",wide(token));
    auto uri=ref new Windows::Foundation::Uri(L"https://aesthetic.computer/api/oskiewar-leaderboard");
    auto content=ref new Windows::Web::Http::HttpStringContent(wide(body),Windows::Storage::Streams::UnicodeEncoding::Utf8,L"application/json");
    const auto httpStatus=std::make_shared<unsigned>(0);
    concurrency::create_task(client->PostAsync(uri,content)).then([client,content,lifetime,httpStatus](Windows::Web::Http::HttpResponseMessage^ response) {
      *httpStatus=static_cast<unsigned>(response->StatusCode);
      if(!response->IsSuccessStatusCode)throw ref new Platform::FailureException();
      return response->Content->ReadAsStringAsync();
    }).then([this,lifetime,generation,httpStatus](concurrency::task<Platform::String^> result) {
      try {
        const auto body=result.get();
        if(body->Length()>65536)throw ref new Platform::FailureException();
        const auto json=JsonObject::Parse(body);
        std::lock_guard<std::mutex> lock(m_mutex);
        if(generation!=m_generation)return;
        m_reportBusy=false;m_pendingReport.clear();m_reportRetries=0;m_reportStatus=json->GetNamedBoolean(L"recorded",false)?"Result recorded":
          json->GetNamedString(L"status",L"")==L"pending"?"Waiting for opponent confirmation":"Result submitted";
      } catch(...) {
        std::lock_guard<std::mutex> lock(m_mutex);
        if(generation!=m_generation)return;
        m_reportBusy=false;
        if(*httpStatus>=400 && *httpStatus<500 && *httpStatus!=408 && *httpStatus!=429) {
          m_pendingReport.clear();m_reportStatus=*httpStatus==409?"Result reports disagreed":
            (*httpStatus==401||*httpStatus==403)?"Sign in again to submit results":"Result rejected";return;
        }
        ++m_reportRetries;
        m_nextReport=GetTickCount64()+(std::min)(30000ULL,1500ULL << (std::min)(m_reportRetries,5u));
        m_reportStatus="Retrying result submission";
      }
    });
    return true;
  }
  std::mutex m_mutex;
  std::wstring m_path;
  std::string m_status = "signed-out", m_handle, m_code, m_secret, m_error, m_token;
  std::string m_reportStatus, m_leaderboardError, m_leaderboard = "{}";
  bool m_reportBusy = false, m_leaderboardBusy = false;
  ULONGLONG m_lastLeaderboard = 0, m_nextReport = 0;
  unsigned m_reportRetries = 0;
  std::string m_pendingReport;
  unsigned m_generation = 0;
  bool m_busy = false;
  ULONGLONG m_deadline = 0, m_nextPoll = 0;
  std::int64_t m_expires = 0;
};
}  // namespace NativeBios
