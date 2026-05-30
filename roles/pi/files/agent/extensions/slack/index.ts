import { createServer } from "node:http";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";
import { spawn } from "node:child_process";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { Type, type Static } from "typebox";
import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StreamableHTTPClientTransport } from "@modelcontextprotocol/sdk/client/streamableHttp.js";
import { UnauthorizedError } from "@modelcontextprotocol/sdk/client/auth.js";

const SLACK_API_BASE = "https://slack.com/api";
const SLACK_MCP_URL = "https://mcp.slack.com/mcp";
const SLACK_MCP_CLIENT_ID = "1601185624273.8899143856786"; // Slack Claude Code plugin client id
const CALLBACK_PORT = 3118;
const CALLBACK_URL = `http://localhost:${CALLBACK_PORT}/callback`;
const AUTH_DIR = join(homedir(), ".pi", "agent", "auth");
const AUTH_FILE = join(AUTH_DIR, "slack-mcp-oauth.json");

function getSlackToken(): string | undefined {
	return (
		process.env.SLACK_BOT_TOKEN ||
		process.env.SLACK_USER_TOKEN ||
		process.env.SLACK_API_TOKEN ||
		process.env.SLACK_MCP_XOXP_TOKEN ||
		process.env.SLACK_MCP_XOXB_TOKEN
	);
}

function authHelpText() {
	return [
		"Slack authentication is not configured for Pi.",
		"Run /slack-login to authenticate Pi against Slack's hosted MCP server via OAuth.",
		"Alternatively, start Pi with SLACK_BOT_TOKEN, SLACK_USER_TOKEN, SLACK_MCP_XOXP_TOKEN, or SLACK_MCP_XOXB_TOKEN set.",
	].join("\n");
}

async function slackApi(method: string, params: Record<string, unknown>, signal?: AbortSignal) {
	const token = getSlackToken();
	if (!token) throw new Error(authHelpText());

	const body = new URLSearchParams();
	for (const [key, value] of Object.entries(params)) {
		if (value === undefined || value === null) continue;
		body.set(key, String(value));
	}

	const response = await fetch(`${SLACK_API_BASE}/${method}`, {
		method: "POST",
		headers: { Authorization: `Bearer ${token}`, "Content-Type": "application/x-www-form-urlencoded" },
		body,
		signal,
	});

	if (!response.ok) throw new Error(`Slack HTTP ${response.status}: ${await response.text()}`);
	const json = (await response.json()) as any;
	if (!json.ok) throw new Error(`Slack API ${method} failed: ${json.error ?? "unknown_error"}`);
	return json;
}

function stripHash(value: string) {
	return value.trim().replace(/^#/, "");
}

function normalize(value: string) {
	return stripHash(value).toLowerCase().replace(/[^a-z0-9]/g, "");
}

function openBrowser(url: string) {
	const opener = process.platform === "darwin" ? "open" : process.platform === "win32" ? "cmd" : "xdg-open";
	const args = process.platform === "win32" ? ["/c", "start", "", url] : [url];
	const child = spawn(opener, args, { detached: true, stdio: "ignore" });
	child.unref();
}

class PersistentOAuthProvider {
	readonly redirectUrl = CALLBACK_URL;
	readonly clientMetadata = {
		client_name: "Pi Slack MCP Client",
		redirect_uris: [CALLBACK_URL],
		grant_types: ["authorization_code", "refresh_token"],
		response_types: ["code"],
		token_endpoint_auth_method: "none",
	};
	private store: any = {};
	private onRedirect?: (url: URL) => void | Promise<void>;

	constructor(onRedirect?: (url: URL) => void | Promise<void>) {
		this.onRedirect = onRedirect;
		try {
			this.store = JSON.parse(readFileSync(AUTH_FILE, "utf8"));
		} catch {
			this.store = {};
		}
	}

	private save() {
		mkdirSync(AUTH_DIR, { recursive: true, mode: 0o700 });
		writeFileSync(AUTH_FILE, JSON.stringify(this.store, null, 2), { mode: 0o600 });
	}

	clientInformation() {
		return this.store.clientInformation ?? { client_id: SLACK_MCP_CLIENT_ID };
	}
	saveClientInformation(clientInformation: any) {
		this.store.clientInformation = clientInformation;
		this.save();
	}
	tokens() {
		return this.store.tokens;
	}
	saveTokens(tokens: any) {
		this.store.tokens = tokens;
		this.save();
	}
	async redirectToAuthorization(authorizationUrl: URL) {
		await this.onRedirect?.(authorizationUrl);
	}
	saveCodeVerifier(codeVerifier: string) {
		this.store.codeVerifier = codeVerifier;
		this.save();
	}
	codeVerifier() {
		if (!this.store.codeVerifier) throw new Error("Missing OAuth code verifier");
		return this.store.codeVerifier;
	}
}

type OAuthCallbackWaiter = {
	promise: Promise<string>;
	close: () => void;
};

function closeServer(server: any) {
	try {
		server?.close();
	} catch {
		// Ignore close races; the server may already be closed.
	}
}

function listen(server: any, host: string) {
	return new Promise<void>((resolve, reject) => {
		const onListening = () => {
			server.off("error", onError);
			resolve();
		};
		const onError = (error: Error) => {
			server.off("listening", onListening);
			reject(error);
		};
		server.once("listening", onListening);
		server.once("error", onError);
		server.listen(CALLBACK_PORT, host);
	});
}

async function createOAuthCallbackWaiter(timeoutMs = 5 * 60 * 1000): Promise<OAuthCallbackWaiter> {
	let settled = false;
	let timeout: ReturnType<typeof setTimeout> | undefined;
	let resolveCode: ((code: string) => void) | undefined;
	let rejectCode: ((error: Error) => void) | undefined;
	const servers: any[] = [];

	const closeServers = () => servers.forEach(closeServer);
	const settle = (value: string | Error) => {
		if (settled) return;
		settled = true;
		if (timeout) clearTimeout(timeout);
		setTimeout(closeServers, 500);
		if (value instanceof Error) rejectCode?.(value);
		else resolveCode?.(value);
	};

	const handler = (req: any, res: any) => {
		const parsed = new URL(req.url || "", `http://localhost:${CALLBACK_PORT}`);
		const code = parsed.searchParams.get("code");
		const error = parsed.searchParams.get("error");
		if (code) {
			res.writeHead(200, { "Content-Type": "text/html" });
			res.end("<h1>Slack authorization successful</h1><p>You can close this window and return to Pi.</p>");
			settle(code);
		} else {
			const message = error ? `OAuth error: ${error}` : "No OAuth code in callback";
			res.writeHead(400, { "Content-Type": "text/plain" });
			res.end(message);
			settle(new Error(message));
		}
	};

	const candidates = [
		{ server: createServer(handler), host: "127.0.0.1" },
		{ server: createServer(handler), host: "::1" },
	];
	const results = await Promise.allSettled(candidates.map(({ server, host }) => listen(server, host)));
	for (let i = 0; i < results.length; i++) {
		if (results[i].status === "fulfilled") servers.push(candidates[i].server);
		else closeServer(candidates[i].server);
	}
	if (servers.length === 0) {
		const firstError = results.find((result) => result.status === "rejected") as PromiseRejectedResult | undefined;
		throw new Error(`Could not start Slack OAuth callback listener on port ${CALLBACK_PORT}: ${firstError?.reason?.message ?? firstError?.reason ?? "unknown error"}`);
	}

	const promise = new Promise<string>((resolve, reject) => {
		resolveCode = resolve;
		rejectCode = reject;
	});
	promise.catch(() => undefined); // The OAuth callback can fail before client.connect throws UnauthorizedError.
	for (const server of servers) {
		server.on("error", (error: Error) => settle(error instanceof Error ? error : new Error(String(error))));
	}
	timeout = setTimeout(() => settle(new Error("Timed out waiting for Slack OAuth callback.")), timeoutMs);

	return {
		promise,
		close: () => {
			if (timeout) clearTimeout(timeout);
			closeServers();
		},
	};
}

let mcpClient: Client | undefined;
let mcpTransport: StreamableHTTPClientTransport | undefined;
let loginInProgress = false;

async function resetMcpClient() {
	const client = mcpClient;
	const transport = mcpTransport;
	mcpClient = undefined;
	mcpTransport = undefined;
	try {
		await client?.close();
	} catch {
		// Ignore cleanup failures.
	}
	try {
		await transport?.close();
	} catch {
		// Ignore cleanup failures.
	}
}

async function connectMcp(interactive: boolean, onAuthUrl?: (url: URL) => void) {
	if (mcpClient) return mcpClient;
	let callbackWaiter: OAuthCallbackWaiter | undefined;
	let authorizationStarted = false;
	const provider = new PersistentOAuthProvider(async (url) => {
		if (!interactive) throw new Error(`${authHelpText()}\nAuthorization URL: ${url.toString()}`);
		callbackWaiter = await createOAuthCallbackWaiter();
		authorizationStarted = true;
		onAuthUrl?.(url);
		openBrowser(url.toString());
	});
	const client = new Client({ name: "pi-slack-extension", version: "1.0.0" }, { capabilities: {} });
	const transport = new StreamableHTTPClientTransport(new URL(SLACK_MCP_URL), { authProvider: provider as any });
	try {
		await client.connect(transport);
	} catch (error) {
		if (!(error instanceof UnauthorizedError) || !interactive || !callbackWaiter || !authorizationStarted) {
			callbackWaiter?.close();
			throw error;
		}
		const code = await callbackWaiter.promise;
		await transport.finishAuth(code);
		callbackWaiter.close();
		return connectMcp(false);
	}
	callbackWaiter?.close();
	mcpClient = client;
	mcpTransport = transport;
	return client;
}

async function callSlackMcp(toolName: string, args: Record<string, unknown>, signal?: AbortSignal) {
	const client = await connectMcp(false);
	return client.callTool({ name: toolName, arguments: args }, undefined, { signal } as any);
}

function contentToText(content: any) {
	if (Array.isArray(content)) return content.map((c) => (c.type === "text" ? c.text : JSON.stringify(c))).join("\n");
	return JSON.stringify(content, null, 2);
}

const readChannelSchema = Type.Object({
	channel: Type.String({ description: "Slack channel ID, e.g. C0123456789" }),
	oldest: Type.Optional(Type.String({ description: "Oldest Slack timestamp to include" })),
	latest: Type.Optional(Type.String({ description: "Latest Slack timestamp to include" })),
	limit: Type.Optional(Type.Number({ description: "Page size, max 1000; use 100 for AWS health workflow" })),
	cursor: Type.Optional(Type.String({ description: "Slack pagination cursor from the previous result" })),
	inclusive: Type.Optional(Type.Boolean({ description: "Whether to include messages exactly at oldest/latest" })),
});
type ReadChannelInput = Static<typeof readChannelSchema>;

const searchChannelsSchema = Type.Object({
	query: Type.String({ description: "Channel name or team name to search for" }),
	limit: Type.Optional(Type.Number({ description: "Maximum matches to return" })),
});
type SearchChannelsInput = Static<typeof searchChannelsSchema>;

const sendMessageSchema = Type.Object({
	channel: Type.String({ description: "Slack channel ID or channel name" }),
	text: Type.String({ description: "Message text" }),
	thread_ts: Type.Optional(Type.String({ description: "Optional thread timestamp" })),
});
type SendMessageInput = Static<typeof sendMessageSchema>;

export default function (pi: ExtensionAPI) {
	async function runSlackLogin(ctx: any) {
		loginInProgress = true;
		try {
			await resetMcpClient();
			await connectMcp(true, (url) => {
				ctx.ui.notify(`Opened Slack OAuth in your browser. If it did not open, visit:\n${url.toString()}`, "info");
			});
			ctx.ui.notify("Slack OAuth completed for Pi.", "info");
		} catch (error) {
			const message = error instanceof Error ? error.message : String(error);
			ctx.ui.notify(`Slack OAuth failed: ${message}`, "error");
		} finally {
			loginInProgress = false;
		}
	}

	pi.registerCommand("slack-login", {
		description: "Authenticate Pi with Slack's hosted MCP server via OAuth",
		handler: async (_args, ctx) => {
			if (loginInProgress) {
				ctx.ui.notify("Slack OAuth is already in progress.", "warning");
				return;
			}
			void runSlackLogin(ctx);
			ctx.ui.notify("Started Slack OAuth in the background; Pi should remain usable while waiting for the browser callback.", "info");
		},
	});

	pi.registerCommand("slack-auth-status", {
		description: "Check whether Pi has Slack auth",
		handler: async (_args, ctx) => {
			const hasWebToken = Boolean(getSlackToken());
			const hasMcpToken = (() => {
				try { return Boolean(JSON.parse(readFileSync(AUTH_FILE, "utf8")).tokens); } catch { return false; }
			})();
			ctx.ui.notify(hasWebToken || hasMcpToken ? "Slack auth is configured for Pi." : authHelpText(), hasWebToken || hasMcpToken ? "info" : "warning");
		},
	});

	pi.registerCommand("slack-auth-help", {
		description: "Show Slack authentication setup for Pi",
		handler: async (_args, ctx) => ctx.ui.notify(authHelpText(), "info"),
	});

	pi.registerCommand("slack-mcp-tools", {
		description: "List tools exposed by Slack MCP",
		handler: async (_args, ctx) => {
			const client = await connectMcp(true);
			const tools = await client.listTools();
			ctx.ui.notify(tools.tools.map((t) => t.name).join("\n"), "info");
		},
	});

	pi.registerTool({
		name: "slack_read_channel",
		label: "Slack: Read Channel",
		description: "Read Slack channel messages using Pi Slack auth. Uses Web API token if present, otherwise Slack hosted MCP OAuth.",
		promptSnippet: "Read messages from a Slack channel by channel ID with optional oldest/latest timestamps and pagination cursor.",
		parameters: readChannelSchema,
		async execute(_toolCallId, params: ReadChannelInput, signal, _onUpdate, ctx) {
			if (!getSlackToken()) {
				const result = await callSlackMcp("slack_read_channel", {
					channel_id: params.channel,
					oldest: params.oldest,
					latest: params.latest,
					limit: params.limit ?? 100,
					cursor: params.cursor,
					response_format: "detailed",
				}, signal ?? ctx.signal);
				return { content: [{ type: "text", text: contentToText(result.content) }], details: result as any };
			}
			const result = await slackApi("conversations.history", { channel: params.channel, oldest: params.oldest, latest: params.latest, limit: params.limit ?? 100, cursor: params.cursor, inclusive: params.inclusive ?? false }, signal ?? ctx.signal);
			const messages = (result.messages ?? []).map((m: any) => ({ ts: m.ts, user: m.user, bot_id: m.bot_id, text: m.text, blocks: m.blocks, attachments: m.attachments }));
			const nextCursor = result.response_metadata?.next_cursor || "";
			return { content: [{ type: "text", text: JSON.stringify({ messages, next_cursor: nextCursor, has_more: Boolean(nextCursor) }, null, 2) }], details: { messages, next_cursor: nextCursor, raw_count: result.messages?.length ?? 0 } };
		},
	});

	pi.registerTool({
		name: "slack_search_channels",
		label: "Slack: Search Channels",
		description: "Search visible Slack public/private channels by name.",
		promptSnippet: "Search Slack channels by team/channel name and return channel IDs and names.",
		parameters: searchChannelsSchema,
		async execute(_toolCallId, params: SearchChannelsInput, signal, _onUpdate, ctx) {
			if (!getSlackToken()) {
				const result = await callSlackMcp("slack_search_channels", params as any, signal ?? ctx.signal);
				return { content: [{ type: "text", text: contentToText(result.content) }], details: result as any };
			}
			const maxMatches = params.limit ?? 20;
			const queryNorm = normalize(params.query);
			const matches: Array<{ id: string; name: string; is_private: boolean; is_archived: boolean; topic?: string; purpose?: string }> = [];
			let cursor = "";
			do {
				const result = await slackApi("conversations.list", { exclude_archived: false, limit: 1000, types: "public_channel,private_channel", cursor }, signal ?? ctx.signal);
				for (const channel of result.channels ?? []) {
					const name = String(channel.name ?? "");
					if (normalize(name).includes(queryNorm) || queryNorm.includes(normalize(name))) {
						matches.push({ id: channel.id, name: `#${name}`, is_private: Boolean(channel.is_private), is_archived: Boolean(channel.is_archived), topic: channel.topic?.value, purpose: channel.purpose?.value });
						if (matches.length >= maxMatches) break;
					}
				}
				cursor = result.response_metadata?.next_cursor || "";
			} while (cursor && matches.length < maxMatches);
			const text = matches.length ? matches.map((m, i) => `${i + 1}. ${m.name} (${m.id})${m.is_archived ? " [archived]" : ""}`).join("\n") : `No channels found for ${params.query}`;
			return { content: [{ type: "text", text }], details: { matches } };
		},
	});

	pi.registerTool({
		name: "slack_send_message",
		label: "Slack: Send Message",
		description: "Send a Slack message after user approval.",
		promptSnippet: "Send a message to a Slack channel after user approval.",
		parameters: sendMessageSchema,
		async execute(_toolCallId, params: SendMessageInput, signal, _onUpdate, ctx) {
			if (!getSlackToken()) {
				const result = await callSlackMcp("slack_send_message", {
					channel_id: params.channel,
					message: params.text,
					thread_ts: params.thread_ts,
				}, signal ?? ctx.signal);
				return { content: [{ type: "text", text: contentToText(result.content) }], details: result as any };
			}
			const result = await slackApi("chat.postMessage", { channel: params.channel, text: params.text, thread_ts: params.thread_ts }, signal ?? ctx.signal);
			return { content: [{ type: "text", text: `Sent Slack message to ${params.channel} at ${result.ts}` }], details: { channel: result.channel, ts: result.ts, message: result.message } };
		},
	});
}
