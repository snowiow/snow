import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { Type } from "typebox";
import { execFile } from "node:child_process";
import { promisify } from "node:util";

const execFileAsync = promisify(execFile);

const EMACS_CONTEXT_ELISP = "\n(progn\n  (require 'json)\n  (defun pi-emacs-context--terminal-buffer-p (buffer)\n    (with-current-buffer buffer\n      (or (derived-mode-p 'term-mode 'vterm-mode 'shell-mode 'eshell-mode 'comint-mode)\n          (string-match-p \"\\\\*\\\\(vterm\\\\|terminal\\\\|shell\\\\|eshell\\\\|ansi-term\\\\|Pi\\\\|pi\\\\)\" (buffer-name buffer)))))\n  (defun pi-emacs-context--buffer-hidden-p (buffer)\n    (string-prefix-p \" \" (buffer-name buffer)))\n  (defun pi-emacs-context--region-active-p (buffer)\n    (with-current-buffer buffer\n      (and mark-active (mark t) (/= (point) (mark t)))))\n  (defun pi-emacs-context--line-number-at (pos)\n    (save-excursion\n      (goto-char pos)\n      (line-number-at-pos pos)))\n  (defun pi-emacs-context--column-at (pos)\n    (save-excursion\n      (goto-char pos)\n      (current-column)))\n  (defun pi-emacs-context--string-limit (s max-chars)\n    (if (and max-chars (> (length s) max-chars))\n        (concat (substring s 0 max-chars)\n                (format \"\\n\u2026[truncated: %d chars total]\" (length s)))\n      s))\n  (defun pi-emacs-context--window-info (window include-text max-chars)\n    (let* ((buffer (window-buffer window))\n           (point (window-point window))\n           (has-region (pi-emacs-context--region-active-p buffer)))\n      (with-current-buffer buffer\n        (let* ((start (when has-region (min point (mark t))))\n               (end (when has-region (max point (mark t))))\n               (text (cond\n                      (has-region (buffer-substring-no-properties start end))\n                      (include-text (buffer-substring-no-properties (point-min) (point-max)))\n                      (t nil))))\n          `((bufferName . ,(buffer-name buffer))\n            (fileName . ,(or buffer-file-name \"\"))\n            (directory . ,default-directory)\n            (majorMode . ,(symbol-name major-mode))\n            (terminalLike . ,(if (pi-emacs-context--terminal-buffer-p buffer) t :json-false))\n            (selectedWindow . ,(if (eq window (selected-window)) t :json-false))\n            (point . ((line . ,(pi-emacs-context--line-number-at point))\n                      (column . ,(pi-emacs-context--column-at point))\n                      (char . ,point)))\n            (region . ,(if has-region\n                           `((active . t)\n                             (startLine . ,(pi-emacs-context--line-number-at start))\n                             (startColumn . ,(pi-emacs-context--column-at start))\n                             (endLine . ,(pi-emacs-context--line-number-at end))\n                             (endColumn . ,(pi-emacs-context--column-at end))\n                             (text . ,(pi-emacs-context--string-limit text max-chars)))\n                         `((active . :json-false))))\n            (text . ,(if (and include-text (not has-region))\n                         (pi-emacs-context--string-limit text max-chars)\n                       \"\")))))))\n  (let* ((mode (or (getenv \"PI_EMACS_CONTEXT_MODE\") \"auto\"))\n         (include-text (string= (or (getenv \"PI_EMACS_CONTEXT_INCLUDE_TEXT\") \"false\") \"true\"))\n         (max-chars (string-to-number (or (getenv \"PI_EMACS_CONTEXT_MAX_CHARS\") \"20000\")))\n         (windows (window-list (selected-frame) 'no-minibuf))\n         (selected (selected-window))\n         (selected-buffer (window-buffer selected))\n         (region-window (catch 'found\n                          (dolist (window windows)\n                            (when (pi-emacs-context--region-active-p (window-buffer window))\n                              (throw 'found window)))\n                          nil))\n         (neighbor-window (catch 'found\n                            (let ((window (next-window selected 'no-minibuf (selected-frame))))\n                              (while (and window (not (eq window selected)))\n                                (let ((buffer (window-buffer window)))\n                                  (unless (or (pi-emacs-context--terminal-buffer-p buffer)\n                                              (pi-emacs-context--buffer-hidden-p buffer))\n                                    (throw 'found window)))\n                                (setq window (next-window window 'no-minibuf (selected-frame)))))\n                            nil))\n         (target-window (cond\n                         ((string= mode \"selection\") region-window)\n                         ((string= mode \"neighbor\") neighbor-window)\n                         ((string= mode \"selected\") selected)\n                         ((and region-window (string= mode \"auto\")) region-window)\n                         ((and (pi-emacs-context--terminal-buffer-p selected-buffer) neighbor-window) neighbor-window)\n                         (t selected)))\n         (reason (cond\n                  ((null target-window) \"none\")\n                  ((eq target-window region-window) \"active-region\")\n                  ((eq target-window neighbor-window) \"neighbor-window\")\n                  ((eq target-window selected) \"selected-window\")\n                  (t \"target\")))\n         (payload `((mode . ,mode)\n                    (reason . ,reason)\n                    (frameName . ,(frame-parameter nil 'name))\n                    (visibleBuffers . ,(vconcat (mapcar (lambda (window)\n                                                          (buffer-name (window-buffer window)))\n                                                        windows)))\n                    (target . ,(if target-window\n                                   (pi-emacs-context--window-info target-window include-text max-chars)\n                                 :json-null)))))\n    (json-encode payload)))";

type EmacsContextParams = {
	mode?: "auto" | "selection" | "neighbor" | "selected";
	includeText?: boolean;
	maxChars?: number;
};

async function getEmacsContext(params: EmacsContextParams, signal?: AbortSignal): Promise<unknown> {
	const env = {
		...process.env,
		PI_EMACS_CONTEXT_MODE: params.mode ?? "auto",
		PI_EMACS_CONTEXT_INCLUDE_TEXT: params.includeText ? "true" : "false",
		PI_EMACS_CONTEXT_MAX_CHARS: String(params.maxChars ?? 20000),
	};

	const { stdout } = await execFileAsync("emacsclient", ["--eval", EMACS_CONTEXT_ELISP], {
		env,
		signal,
		maxBuffer: 1024 * 1024 * 5,
	});

	const printed = stdout.trim();
	const jsonText = JSON.parse(printed) as string;
	return JSON.parse(jsonText);
}

function shouldAutoInject(prompt: string): boolean {
	return /\b(selection|selected|highlight(?:ed)?|mark(?:ed)?|region|current buffer|open buffer|buffer next to|next buffer|adjacent buffer|this buffer|this code|the buffer)\b/i.test(prompt);
}

export default function emacsContextExtension(pi: ExtensionAPI): void {
	pi.registerTool({
		name: "get_emacs_context",
		label: "Get Emacs Context",
		description: "Return the active Emacs region, the selected Emacs buffer, or the non-terminal buffer next to Pi.",
		promptSnippet: "Inspect the active Emacs selection or neighboring/selected Emacs buffer metadata and text.",
		promptGuidelines: [
			"Use get_emacs_context when the user refers to highlighted/marked/selected text in Emacs, the current Emacs buffer, or the buffer/window next to Pi.",
			"Prefer get_emacs_context mode=auto first; if it returns a fileName, use read for larger file contents instead of requesting huge buffer text.",
		],
		parameters: Type.Object({
			mode: Type.Optional(Type.Union([
				Type.Literal("auto"),
				Type.Literal("selection"),
				Type.Literal("neighbor"),
				Type.Literal("selected"),
			], { description: "auto = active region, else neighboring non-terminal window if Pi is selected, else selected window." })),
			includeText: Type.Optional(Type.Boolean({ description: "Include full target buffer text when no active region exists. Regions are always included." })),
			maxChars: Type.Optional(Type.Number({ description: "Maximum characters of region/buffer text to return.", default: 20000 })),
		}),
		async execute(_toolCallId, params, signal) {
			try {
				const context = await getEmacsContext(params as EmacsContextParams, signal);
				return {
					content: [{ type: "text", text: JSON.stringify(context, null, 2) }],
					details: context as Record<string, unknown>,
				};
			} catch (error) {
				const message = error instanceof Error ? error.message : String(error);
				return {
					content: [{ type: "text", text: `Failed to query Emacs via emacsclient: ${message}` }],
					isError: true,
				};
			}
		},
	});

	pi.registerCommand("emacs-context", {
		description: "Show the Emacs active region/current/neighboring buffer context that Pi can see",
		handler: async (args, ctx) => {
			const includeText = /\b(text|full|content)\b/i.test(args ?? "");
			const mode = /\bselection\b/i.test(args ?? "")
				? "selection"
				: /\bneighbor\b/i.test(args ?? "")
					? "neighbor"
					: /\bselected\b/i.test(args ?? "")
						? "selected"
						: "auto";
			try {
				const context = await getEmacsContext({ mode: mode as EmacsContextParams["mode"], includeText, maxChars: 20000 });
				ctx.ui.notify(JSON.stringify(context, null, 2), "info");
			} catch (error) {
				ctx.ui.notify(`Failed to query Emacs: ${error instanceof Error ? error.message : String(error)}`, "error");
			}
		},
	});

	pi.on("before_agent_start", async (event) => {
		if (!shouldAutoInject(event.prompt)) return;
		try {
			const context = await getEmacsContext({ mode: "auto", includeText: true, maxChars: 20000 });
			return {
				message: {
					customType: "emacs-context",
					content: `Emacs context automatically captured because the prompt referred to a selection/buffer:\n${JSON.stringify(context, null, 2)}`,
					display: true,
					details: context,
				},
			};
		} catch {
			// Keep the normal prompt flowing; the model can still try the tool and show the error.
		}
	});
}
