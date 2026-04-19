local utils = require("omni.repl.utils")

local Module = {}

function Module.new(state, notify, output)
  local trim = utils.trim
  local ends_with_newline = utils.ends_with_newline
  local strip_ansi = utils.strip_ansi
  local append_lines = output.append_lines
  local flush_partial = output.flush_partial
  local format_eval_value = output.format_eval_value
  local handle_stream = output.handle_stream
  local open_output = output.open_output
  local place_eval_annotation = output.place_eval_annotation
  local api = {}

  local function shell_command(config)
    if type(config.cmd) == "string" then
      return vim.split(config.cmd, " ", { trimempty = true })
    end
    return config.cmd
  end

  local function eval_command(config)
    if type(config.eval.cmd) == "string" then
      return vim.split(config.eval.cmd, " ", { trimempty = true })
    end
    return config.eval.cmd
  end

  local function repl_mode(config)
    if config.repl and config.repl.mode then
      return config.repl.mode
    end
    return "text"
  end

  local function discovery_config(config)
    local repl_config = config and config.repl or {}
    local discovery = repl_config.discovery
    if type(discovery) ~= "table" then
      discovery = {}
    end
    return {
      enabled = discovery.enabled ~= false,
      host = type(discovery.host) == "string" and discovery.host ~= "" and discovery.host or "127.0.0.1",
      port_file = type(discovery.port_file) == "string" and discovery.port_file ~= "" and discovery.port_file or ".omni-repl-port",
    }
  end

  local function root_markers(config)
    local lsp_config = config and config.lsp or {}
    if type(lsp_config.root_markers) == "table" and not vim.tbl_isempty(lsp_config.root_markers) then
      return lsp_config.root_markers
    end
    return { "omni.toml", "project.json", ".git" }
  end

  local function current_buffer_dir()
    local path = vim.api.nvim_buf_get_name(0)
    if type(path) == "string" and path ~= "" then
      local normalized = vim.fs.normalize(path)
      if vim.fn.isdirectory(normalized) == 1 then
        return normalized
      end
      return vim.fs.dirname(normalized)
    end
    return vim.fn.getcwd()
  end

  local function project_root(config)
    local start = current_buffer_dir()
    local match = vim.fs.find(root_markers(config), {
      path = start,
      upward = true,
      stop = vim.loop.os_homedir(),
    })[1]
    if match then
      return vim.fs.dirname(match)
    end
    return start
  end

  local function discover_tcp_endpoint(config)
    local discovery = discovery_config(config)
    if not discovery.enabled then
      return nil
    end

    local root = project_root(config)
    local candidates = {}
    if root and root ~= "" then
      table.insert(candidates, vim.fs.normalize(root .. "/" .. discovery.port_file))
    end

    local start = current_buffer_dir()
    local found = vim.fs.find(discovery.port_file, {
      path = start,
      upward = true,
      stop = vim.loop.os_homedir(),
    })[1]
    if found then
      found = vim.fs.normalize(found)
      local seen = false
      for _, candidate in ipairs(candidates) do
        if candidate == found then
          seen = true
          break
        end
      end
      if not seen then
        table.insert(candidates, found)
      end
    end

    for _, path in ipairs(candidates) do
      if vim.fn.filereadable(path) == 1 then
        local lines = vim.fn.readfile(path, "", 1)
        local raw_port = trim(lines[1] or "")
        local port = tonumber(raw_port)
        if port and port > 0 and port <= 65535 then
          return {
            host = discovery.host,
            port = math.floor(port),
            path = path,
          }
        end
      end
    end

    return nil
  end

  local append_eval_result

  local function reset_request_state()
    state.stdout_partial = ""
    state.stderr_partial = ""
    state.request_seq = 0
    state.pending = {}
    state.deferred_requests = {}
    state.session_id = nil
  end

  local function next_request_id()
    state.request_seq = state.request_seq + 1
    return tostring(state.request_seq)
  end

  local function append_prefixed_chunks(prefix, text)
    if type(text) ~= "string" or text == "" then
      return
    end
    local lines = {}
    for _, line in ipairs(vim.split(text, "\n", { plain = true })) do
      table.insert(lines, prefix .. line)
    end
    append_lines(lines)
  end

  local function flush_server_deferred_requests(config)
    if state.protocol ~= "server_json" or not state.session_id then
      return
    end
    local queued = state.deferred_requests
    state.deferred_requests = {}
    for _, item in ipairs(queued) do
      local request_id = next_request_id()
      state.pending[request_id] = {
        kind = "server_eval",
        label = item.label,
        payload = item.payload,
        annotation_ctx = item.annotation_ctx,
        output = {},
      }
      local request = vim.json.encode({
        id = request_id,
        op = "eval",
        session = state.session_id,
        code = item.payload,
        mode = item.mode,
      })
      local sent = vim.fn.chansend(state.job_id, request .. "\n")
      if sent == 0 then
        state.pending[request_id] = nil
        append_lines({
          string.format(">> %s", item.label),
          item.payload,
          "!! failed to send request to omni repl server",
        })
      end
    end
  end

  local function handle_server_message(decoded)
    local event = decoded.event
    local request_id = decoded.id
    local pending = request_id and state.pending[request_id] or nil

    if event == "session" and pending and pending.kind == "clone" then
      pending.session_id = decoded.session
      return
    end

    if event == "out" or event == "err" then
      if pending and pending.kind == "server_eval" then
        local prefix = event == "err" and "stderr| " or "stdout| "
        local text = tostring(decoded.text or "")
        if text ~= "" then
          for _, line in ipairs(vim.split(text, "\n", { plain = true })) do
            table.insert(pending.output, prefix .. strip_ansi(line))
          end
        end
        return
      end
      append_prefixed_chunks(event == "err" and "stderr| " or "stdout| ", strip_ansi(tostring(decoded.text or "")))
      return
    end

    if event == "value" and pending and pending.kind == "server_eval" then
      pending.result = { ok = true, value = decoded.value }
      return
    end

    if event == "interrupted" and pending and pending.kind == "server_eval" then
      state.pending[request_id] = nil
      local result = {
        ok = false,
        error = {
          message = "evaluation interrupted",
        },
      }
      if #pending.output > 0 then
        append_lines(pending.output)
      end
      append_eval_result((pending.annotation_ctx and pending.annotation_ctx.config) or nil,
        pending.label,
        pending.payload,
        result,
        pending.annotation_ctx)
      return
    end

    if event == "error" and pending and pending.kind == "server_eval" then
      state.pending[request_id] = nil
      local error_info = decoded.error or {}
      local result = {
        ok = false,
        error = {
          message = error_info.message or "unknown evaluation error",
          range = error_info.range,
        },
      }
      if #pending.output > 0 then
        append_lines(pending.output)
      end
      append_eval_result((pending.annotation_ctx and pending.annotation_ctx.config) or nil,
        pending.label,
        pending.payload,
        result,
        pending.annotation_ctx)
      return
    end

    if event == "done" and pending and pending.kind == "clone" then
      state.pending[request_id] = nil
      state.session_id = pending.session_id
      if state.session_id then
        append_lines({ string.format("[omni repl] connected to tcp server session %s", state.session_id) })
        flush_server_deferred_requests(pending.config)
      else
        append_lines({ "!! omni repl server did not return a session id" })
      end
      return
    end

    if event == "done" and pending and pending.kind == "server_eval" then
      state.pending[request_id] = nil
      local result = pending.result or { ok = true, value = "#<void>" }
      if #pending.output > 0 then
        append_lines(pending.output)
      end
      append_eval_result((pending.annotation_ctx and pending.annotation_ctx.config) or nil,
        pending.label,
        pending.payload,
        result,
        pending.annotation_ctx)
      return
    end

    if event == "error" then
      local error_info = decoded.error or {}
      local message = error_info.message or "unknown repl server error"
      append_lines({ string.format("!! %s", message) })
      if request_id and pending then
        state.pending[request_id] = nil
      end
      return
    end
  end

  local function handle_legacy_json_message(decoded, line)
    local request_id = decoded.id
    local pending = request_id and state.pending[request_id] or nil
    if pending then
      state.pending[request_id] = nil
      append_eval_result((pending.annotation_ctx and pending.annotation_ctx.config) or nil,
        pending.label,
        pending.payload,
        decoded,
        pending.annotation_ctx)
      return
    end

    local error_info = decoded.error or {}
    local message = error_info.message or "unknown repl error"
    append_lines({ string.format("!! %s", message) })
  end

  local function handle_json_message(line)
    local ok, decoded = pcall(vim.json.decode, line)
    if not ok or type(decoded) ~= "table" then
      append_lines({ strip_ansi(line) })
      return
    end

    if state.protocol == "server_json" then
      handle_server_message(decoded)
      return
    end

    flush_partial("stderr", "stderr| ")
    handle_legacy_json_message(decoded, line)
  end

  local function handle_json_stream(data)
    if not data or vim.tbl_isempty(data) then
      return
    end

    local partial = state.stdout_partial
    for index, chunk in ipairs(data) do
      local text = chunk or ""
      if index == 1 then
        text = partial .. text
      end

      local is_last = index == #data
      if is_last and chunk ~= "" then
        partial = text
      else
        partial = ""
        text = strip_ansi(text)
        if text ~= "" then
          handle_json_message(text)
        end
      end
    end

    state.stdout_partial = partial
  end

  function api.start(config, opts)
    opts = opts or {}
    local restore_win = opts.focus == false and vim.api.nvim_get_current_win() or nil

    if state.job_id and (state.channel_kind ~= "job" or vim.fn.jobwait({ state.job_id }, 0)[1] == -1) then
      open_output(config)
      if restore_win and vim.api.nvim_win_is_valid(restore_win) then
        vim.api.nvim_set_current_win(restore_win)
      end
      return state.job_id
    end

    open_output(config)
    if restore_win and vim.api.nvim_win_is_valid(restore_win) then
      vim.api.nvim_set_current_win(restore_win)
    end
    append_lines({
      repl_mode(config) == "json"
        and "[omni repl] starting structured session..."
        or "[omni repl] starting...",
    })

    reset_request_state()

    local endpoint = repl_mode(config) == "json" and discover_tcp_endpoint(config) or nil
    if endpoint then
      local channel_id = vim.fn.sockconnect("tcp", string.format("%s:%d", endpoint.host, endpoint.port), {
        rpc = false,
        on_data = function(_, data)
          vim.schedule(function()
            if data and #data == 1 and data[1] == "" then
              if state.stdout_partial ~= "" then
                handle_json_message(state.stdout_partial)
                state.stdout_partial = ""
              end
              append_lines({ string.format("[omni repl] tcp server disconnected (%s:%d)", endpoint.host, endpoint.port) })
              state.job_id = nil
              state.channel_kind = nil
              state.protocol = "legacy_json"
              reset_request_state()
              return
            end
            handle_json_stream(data)
          end)
        end,
      })
      if channel_id > 0 then
        state.job_id = channel_id
        state.channel_kind = "socket"
        state.protocol = "server_json"
        append_lines({ string.format("[omni repl] connecting to tcp server via %s", endpoint.path) })
        local request_id = next_request_id()
        state.pending[request_id] = {
          kind = "clone",
          config = config,
        }
        local request = vim.json.encode({
          id = request_id,
          op = "clone",
        })
        local sent = vim.fn.chansend(state.job_id, request .. "\n")
        if sent == 0 then
          state.pending[request_id] = nil
          vim.fn.chanclose(state.job_id)
          state.job_id = nil
          state.channel_kind = nil
          state.protocol = "legacy_json"
          append_lines({ "[omni repl] failed to initialize tcp server session; falling back to local process" })
        else
          return channel_id
        end
      else
        append_lines({ string.format("[omni repl] failed to connect to discovered tcp server %s:%d; falling back to local process", endpoint.host, endpoint.port) })
      end
    end

    local json_mode = repl_mode(config) == "json"
    local job_id = vim.fn.jobstart(shell_command(config), {
      pty = not json_mode,
      on_stdout = function(_, data)
        vim.schedule(function()
          if json_mode then
            handle_json_stream(data)
          else
            handle_stream("stdout", nil, data)
          end
        end)
      end,
      on_stderr = function(_, data)
        vim.schedule(function()
          handle_stream("stderr", "stderr| ", data)
        end)
      end,
      on_exit = function(_, code)
        vim.schedule(function()
          if json_mode then
            if state.stdout_partial ~= "" then
              handle_json_message(state.stdout_partial)
              state.stdout_partial = ""
            end
          else
            flush_partial("stdout")
          end
          flush_partial("stderr", "stderr| ")
          if json_mode then
            for _, pending in pairs(state.pending) do
              if pending.label and pending.payload then
                append_lines({
                  string.format(">> %s", pending.label),
                  pending.payload,
                  string.format("!! omni structured repl exited before replying (code %d)", code),
                })
              end
            end
          end
          append_lines({ string.format("[omni repl] exited with code %d", code) })
          state.job_id = nil
          state.channel_kind = nil
          state.protocol = "legacy_json"
          reset_request_state()
        end)
      end,
    })

    if job_id <= 0 then
      notify("failed to start omni REPL job", vim.log.levels.ERROR)
      return nil
    end

    state.job_id = job_id
    state.channel_kind = "job"
    state.protocol = "legacy_json"
    return job_id
  end

  function api.stop()
    if not state.job_id then
      return
    end
    if state.channel_kind == "socket" then
      vim.fn.chanclose(state.job_id)
    else
      vim.fn.jobstop(state.job_id)
    end
    state.job_id = nil
    state.channel_kind = nil
    state.protocol = "legacy_json"
    reset_request_state()
  end

  function api.restart(config)
    api.stop()
    return api.start(config)
  end

  append_eval_result = function(config, label, payload, result, annotation_ctx)
    local lines = {
      string.format(">> %s", label),
      payload,
    }

    if result.ok then
      local display = format_eval_value(config, result.value)
      local formatted_lines = vim.split(display, "\n", { plain = true })
      if #formatted_lines <= 1 then
        table.insert(lines, string.format("=> %s", display))
      else
        table.insert(lines, "=>")
        for _, line in ipairs(formatted_lines) do
          table.insert(lines, line)
        end
      end
    else
      local error_info = result.error or {}
      local message = error_info.message or "unknown evaluation error"
      local range = error_info.range or {}
      local start = range.start or {}
      if type(start.line) == "number" and type(start.character) == "number" then
        message = string.format("%s (%d:%d)", message, start.line + 1, start.character + 1)
      end
      table.insert(lines, string.format("!! %s", message))
    end

    local config = annotation_ctx and annotation_ctx.config
    if config then
      place_eval_annotation(config, annotation_ctx, label, result)
    end
    append_lines(lines)
  end

  local function eval_once(config, text, label, annotation_ctx)
    local payload = trim(text or "")
    if payload == "" then
      notify("nothing to evaluate", vim.log.levels.WARN)
      return false
    end

    open_output(config)

    local result = vim.system(vim.list_extend(eval_command(config), { payload }), { text = true }):wait()
    if result.code == 0 or result.code == 1 then
      local ok, decoded = pcall(vim.json.decode, result.stdout)
      if ok and type(decoded) == "table" then
        local stderr = trim(result.stderr or "")
        if stderr ~= "" then
          for _, line in ipairs(vim.split(stderr, "\n", { plain = true, trimempty = true })) do
            append_lines({ string.format("stderr| %s", strip_ansi(line)) })
          end
        end
        append_eval_result(config, label, payload, decoded, annotation_ctx)
        return decoded.ok == true
      end
    end

    local stderr = trim(result.stderr or "")
    local stdout = trim(result.stdout or "")
    local message = stderr ~= "" and stderr or stdout
    if message == "" then
      message = string.format("omni --eval failed with exit code %d", result.code)
    end
    append_lines({
      string.format(">> %s", label),
      payload,
      string.format("!! %s", message),
    })
    return false
  end

  local function send_text(config, text, label, mode_override, annotation_ctx)
    local payload = trim(text or "")
    if payload == "" then
      notify("nothing to send", vim.log.levels.WARN)
      return
    end

    if not state.job_id then
      if not config.auto_start then
        notify("Omni REPL is not running", vim.log.levels.WARN)
        return
      end
      if not api.start(config) then
        return
      end
    end

    if state.protocol == "server_json" then
      if not state.session_id then
        table.insert(state.deferred_requests, {
          label = label,
          payload = payload,
          annotation_ctx = annotation_ctx,
          mode = mode_override or (label == "buffer" and "program" or "expr"),
        })
        append_lines({ "[omni repl] queued request until tcp server session is ready" })
        return
      end

      local request_id = next_request_id()
      state.pending[request_id] = {
        kind = "server_eval",
        label = label,
        payload = payload,
        annotation_ctx = annotation_ctx,
        output = {},
      }
      local mode = mode_override or (label == "buffer" and "program" or "expr")
      local request = vim.json.encode({
        id = request_id,
        op = "eval",
        session = state.session_id,
        code = payload,
        mode = mode,
      })
      local sent = vim.fn.chansend(state.job_id, request .. "\n")
      if sent == 0 then
        state.pending[request_id] = nil
        append_lines({
          string.format(">> %s", label),
          payload,
          "!! failed to send request to omni repl server",
        })
      end
      return
    end

    if repl_mode(config) == "json" then
      local request_id = next_request_id()
      state.pending[request_id] = {
        kind = "legacy_eval",
        label = label,
        payload = payload,
        annotation_ctx = annotation_ctx,
      }

      local mode = mode_override or (label == "buffer" and "program" or "expr")
      local request = vim.json.encode({
        id = request_id,
        input = payload,
        mode = mode,
      })
      local sent = vim.fn.chansend(state.job_id, request .. "\n")
      if sent == 0 then
        state.pending[request_id] = nil
        append_lines({
          string.format(">> %s", label),
          payload,
          "!! failed to send request to omni structured repl",
        })
      end
      return
    end

    append_lines({ string.format(">> %s", label), payload })
    vim.fn.chansend(state.job_id, payload)
  if not ends_with_newline(payload) then
    vim.fn.chansend(state.job_id, "\n")
  end
  end

  api.send_text = send_text
  api.eval_once = eval_once
  return api
end

return Module
