import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

type IntentKind =
	| "file-delete"
	| "git-publish"
	| "git-destructive"
	| "github-pr"
	| "github-repo-release"
	| "package-publish"
	| "privileged"
	| "permission-change"
	| "remote-code-exec"
	| "disk-destructive"
	| "docker-destructive"
	| "kubernetes-mutation"
	| "helm-mutation"
	| "infra-mutation"
	| "aws-mutation"
	| "service-mutation"
	| "sensitive-write";

type Finding = {
	name: string;
	reason: string;
	kind?: IntentKind;
};

type Rule = Finding & {
	pattern: RegExp;
};

type DeniedDecision = {
	kind: IntentKind;
	targets: string[];
	findings: string[];
	source: string;
	deniedAt: number;
};

const dangerousCommandRules: Rule[] = [
	{
		name: "rm/delete command",
		reason: "Removes files or directories.",
		kind: "file-delete",
		pattern: /(?:^|[;&|()\n]\s*)(?:sudo\s+|doas\s+)?(?:command\s+)?rm\b/i,
	},
	{
		name: "shred command",
		reason: "Securely deletes file contents.",
		kind: "file-delete",
		pattern: /(?:^|[;&|()\n]\s*)(?:sudo\s+|doas\s+)?shred\b/i,
	},
	{
		name: "find -delete",
		reason: "Deletes files matched by find.",
		kind: "file-delete",
		pattern: /\bfind\b[\s\S]*\s-delete\b/i,
	},
	{
		name: "xargs rm",
		reason: "Pipes a generated file list into rm.",
		kind: "file-delete",
		pattern: /\bxargs\b[\s\S]*\brm\b/i,
	},
	{
		name: "scripted file deletion",
		reason: "Deletes files through a scripting language API such as unlink, rmdir, rm, or rmtree.",
		kind: "file-delete",
		pattern:
			/\b(?:python3?|node|ruby|perl)\b[\s\S]*(?:\.\s*(?:unlink|rmdir)\s*\(|\bos\.(?:remove|unlink|rmdir)\s*\(|\bshutil\.rmtree\s*\(|\bfs\.(?:rm|unlink|rmdir)(?:Sync)?\s*\(|\bFile\.(?:delete|unlink)\s*\(|\bunlink\s+(?:["'`]|[A-Za-z0-9_./~-]))/i,
	},
	{
		name: "git push",
		reason: "Publishes commits/tags to a remote repository.",
		kind: "git-publish",
		pattern: /\bgit\s+push\b/i,
	},
	{
		name: "git history/worktree destructive command",
		reason: "Can discard local changes, remove untracked files, or delete refs.",
		kind: "git-destructive",
		pattern: /\bgit\s+(?:clean\b|reset\s+--hard\b|branch\s+-D\b|tag\s+-d\b|checkout\b[\s\S]*(?:\s-f\b|\s--force\b)|restore\b[\s\S]*(?:\s-f\b|\s--force\b))/i,
	},
	{
		name: "GitHub PR operation",
		reason: "Creates, updates, comments on, reviews, closes, or merges GitHub PRs.",
		kind: "github-pr",
		pattern: /\bgh\s+pr\b/i,
	},
	{
		name: "GitHub repo/release operation",
		reason: "Can publish, modify, delete, archive, or transfer GitHub resources.",
		kind: "github-repo-release",
		pattern: /\bgh\s+(?:release|repo)\s+(?:create|delete|edit|archive|rename|transfer|upload|deploy-key)\b/i,
	},
	{
		name: "package publish/unpublish",
		reason: "Publishes or removes packages from a registry.",
		kind: "package-publish",
		pattern: /\b(?:npm\s+(?:publish|unpublish)|pnpm\s+publish|yarn\s+(?:npm\s+)?publish)\b/i,
	},
	{
		name: "privileged command",
		reason: "Runs a command with elevated privileges or as another user.",
		kind: "privileged",
		pattern: /(?:^|[;&|()\n]\s*)(?:sudo|doas|su)\b/i,
	},
	{
		name: "permission/ownership change",
		reason: "Can recursively or broadly change permissions/ownership.",
		kind: "permission-change",
		pattern: /\b(?:chmod\b[\s\S]*(?:\s-R\b|\s777\b|\s[0-7]*7[0-7]*\b|\+[rwx]*w)|chown\b[\s\S]*(?:\s-R\b|\s--recursive\b))/i,
	},
	{
		name: "curl/wget piped to interpreter",
		reason: "Downloads and immediately executes remote code.",
		kind: "remote-code-exec",
		pattern: /\b(?:curl|wget)\b[\s\S]*\|\s*(?:sudo\s+|doas\s+)?(?:sh|bash|zsh|fish|python|python3|ruby|perl|node)\b/i,
	},
	{
		name: "disk/partition command",
		reason: "Can overwrite disks, filesystems, or partitions.",
		kind: "disk-destructive",
		pattern: /(?:\bdd\b[\s\S]*\bof=\/dev\/|\b(?:mkfs|fdisk|parted|sfdisk|wipefs)\b)/i,
	},
	{
		name: "Docker destructive/publish command",
		reason: "Can delete containers/images/volumes or publish images.",
		kind: "docker-destructive",
		pattern: /\bdocker\s+(?:push\b|rm\b|rmi\b|volume\s+rm\b|system\s+prune\b|compose\s+down\b)/i,
	},
	{
		name: "Kubernetes mutation",
		reason: "Mutates Kubernetes cluster state.",
		kind: "kubernetes-mutation",
		pattern: /\bkubectl\s+(?:apply|delete|patch|replace|scale|drain|cordon|uncordon|rollout\s+restart)\b/i,
	},
	{
		name: "Helm mutation",
		reason: "Installs, upgrades, rolls back, or removes Kubernetes releases.",
		kind: "helm-mutation",
		pattern: /\bhelm\s+(?:install|upgrade|uninstall|delete|rollback)\b/i,
	},
	{
		name: "Terraform/OpenTofu mutation",
		reason: "Applies or destroys infrastructure state.",
		kind: "infra-mutation",
		pattern: /\b(?:terraform|tofu)\s+(?:apply|destroy|import|taint|untaint|state\s+(?:rm|mv|push))\b/i,
	},
	{
		name: "AWS destructive CLI action",
		reason: "Can delete, terminate, stop, revoke, detach, or modify AWS resources.",
		kind: "aws-mutation",
		pattern: /\baws\s+[a-z0-9-]+\s+(?:delete|terminate|stop|reboot|revoke|detach|deregister|disable|modify|put|update)-[a-z0-9-]+\b/i,
	},
	{
		name: "service/system mutation",
		reason: "Starts, stops, restarts, disables, or reconfigures system services.",
		kind: "service-mutation",
		pattern: /\b(?:systemctl|service)\s+(?:start|stop|restart|reload|disable|enable|mask|unmask)\b/i,
	},
];

const sensitivePathRules: Rule[] = [
	{
		name: "environment/secrets file",
		reason: "Writes to a file that commonly contains secrets.",
		kind: "sensitive-write",
		pattern: /(?:^|\/)(?:\.env(?:\..*)?|\.npmrc|\.pypirc|credentials|config)$/i,
	},
	{
		name: "SSH/GPG/Kubernetes/AWS secret path",
		reason: "Writes to sensitive local credentials or configuration.",
		kind: "sensitive-write",
		pattern: /(?:^|\/)(?:\.ssh|\.gnupg|\.aws|\.kube)(?:\/|$)/i,
	},
	{
		name: "git internals",
		reason: "Writes inside .git internals.",
		kind: "sensitive-write",
		pattern: /(?:^|\/)\.git(?:\/|$)/i,
	},
];

function uniqueFindings(findings: Finding[]): Finding[] {
	const seen = new Set<string>();
	return findings.filter((finding) => {
		if (seen.has(finding.name)) return false;
		seen.add(finding.name);
		return true;
	});
}

function uniqueStrings(values: string[]): string[] {
	const seen = new Set<string>();
	return values.filter((value) => {
		if (!value || seen.has(value)) return false;
		seen.add(value);
		return true;
	});
}

function normalizeTargetPath(path: string): string {
	let normalized = path.trim().replace(/^@/, "").replace(/\\/g, "/").replace(/\/+$/g, "");
	normalized = normalized.replace(/^(?:\.\/)+/, "");
	return normalized || path.trim();
}

function shellWords(input: string): string[] {
	const words: string[] = [];
	let current = "";
	let quote: "'" | '"' | "`" | undefined;
	let escaped = false;

	for (const ch of input) {
		if (escaped) {
			current += ch;
			escaped = false;
			continue;
		}

		if (ch === "\\" && quote !== "'") {
			escaped = true;
			continue;
		}

		if (quote) {
			if (ch === quote) {
				quote = undefined;
			} else {
				current += ch;
			}
			continue;
		}

		if (ch === "'" || ch === '"' || ch === "`") {
			quote = ch;
			continue;
		}

		if (/\s/.test(ch)) {
			if (current) {
				words.push(current);
				current = "";
			}
			continue;
		}

		current += ch;
	}

	if (current) words.push(current);
	return words;
}

function extractRmTargets(command: string): string[] {
	const targets: string[] = [];
	const rmPattern = /(?:^|[;&|()\n]\s*)(?:sudo\s+|doas\s+)?(?:command\s+)?rm\b([\s\S]*?)(?=$|[;&|()\n])/gi;
	let match: RegExpExecArray | null;

	while ((match = rmPattern.exec(command)) !== null) {
		for (const word of shellWords(match[1] ?? "")) {
			if (word === "--") continue;
			if (word.startsWith("-")) continue;
			targets.push(normalizeTargetPath(word));
		}
	}

	return uniqueStrings(targets);
}

function extractScriptDeletionTargets(command: string): string[] {
	const targets: string[] = [];
	const patterns = [
		/\b(?:Path|PurePath)\s*\(\s*(["'`])([^"'`]+)\1\s*\)\s*\.\s*(?:unlink|rmdir)\s*\(/gi,
		/\bos\.(?:remove|unlink|rmdir)\s*\(\s*(["'`])([^"'`]+)\1/gi,
		/\bshutil\.rmtree\s*\(\s*(["'`])([^"'`]+)\1/gi,
		/\bfs\.(?:rm|unlink|rmdir)(?:Sync)?\s*\(\s*(["'`])([^"'`]+)\1/gi,
		/\bFile\.(?:delete|unlink)\s*\(\s*(["'`])([^"'`]+)\1/gi,
		/\bunlink\s+(["'`])([^"'`]+)\1/gi,
	];

	for (const pattern of patterns) {
		let match: RegExpExecArray | null;
		while ((match = pattern.exec(command)) !== null) {
			targets.push(normalizeTargetPath(match[2] ?? ""));
		}
	}

	return uniqueStrings(targets);
}

function extractDeletionTargets(command: string): string[] {
	return uniqueStrings([...extractRmTargets(command), ...extractScriptDeletionTargets(command)]);
}

function inspectCommand(command: string): Finding[] {
	const findings = dangerousCommandRules
		.filter((rule) => rule.pattern.test(command))
		.map(({ name, reason, kind }) => ({ name, reason, kind }));

	if (extractDeletionTargets(command).length > 0 && !findings.some((finding) => finding.kind === "file-delete")) {
		findings.push({
			name: "file deletion",
			reason: "Deletes files or directories.",
			kind: "file-delete",
		});
	}

	return uniqueFindings(findings);
}

function inspectPath(path: string): Finding[] {
	return uniqueFindings(
		sensitivePathRules
			.filter((rule) => rule.pattern.test(path))
			.map(({ name, reason, kind }) => ({ name, reason, kind })),
	);
}

function findingKinds(findings: Finding[]): IntentKind[] {
	return uniqueStrings(findings.map((finding) => finding.kind).filter((kind): kind is IntentKind => Boolean(kind))) as IntentKind[];
}

function sameTarget(a: string, b: string): boolean {
	const left = normalizeTargetPath(a);
	const right = normalizeTargetPath(b);
	return left === right || left.endsWith(`/${right}`) || right.endsWith(`/${left}`);
}

function decisionTargets(kind: IntentKind, source: string): string[] {
	if (kind === "file-delete") return extractDeletionTargets(source);
	if (kind === "sensitive-write") return [normalizeTargetPath(source)];
	return [];
}

function makeDeniedDecisions(source: string, findings: Finding[]): DeniedDecision[] {
	return findingKinds(findings).map((kind) => ({
		kind,
		targets: decisionTargets(kind, source),
		findings: findings.map((finding) => finding.name),
		source,
		deniedAt: Date.now(),
	}));
}

function rememberDeniedDecision(deniedDecisions: DeniedDecision[], source: string, findings: Finding[]) {
	for (const decision of makeDeniedDecisions(source, findings)) {
		const alreadyKnown = deniedDecisions.some(
			(existing) =>
				existing.kind === decision.kind &&
				existing.targets.length === decision.targets.length &&
				existing.targets.every((target) => decision.targets.some((newTarget) => sameTarget(target, newTarget))),
		);
		if (!alreadyKnown) deniedDecisions.push(decision);
	}

	if (deniedDecisions.length > 100) deniedDecisions.splice(0, deniedDecisions.length - 100);
}

function matchingDeniedDecision(
	deniedDecisions: DeniedDecision[],
	findings: Finding[],
	source: string,
): DeniedDecision | undefined {
	const kinds = new Set(findingKinds(findings));
	const deletionTargets = extractDeletionTargets(source);
	const sourceTarget = normalizeTargetPath(source);

	return deniedDecisions.find((decision) => {
		if (!kinds.has(decision.kind)) {
			// A denied deletion of a path should also prevent write/edit attempts to that same path.
			if (decision.kind !== "file-delete" || !kinds.has("sensitive-write")) return false;
		}

		if (decision.kind === "file-delete") {
			if (decision.targets.length === 0) return true;
			const currentTargets = deletionTargets.length > 0 ? deletionTargets : [sourceTarget];
			return decision.targets.some((deniedTarget) => currentTargets.some((target) => sameTarget(deniedTarget, target)));
		}

		if (decision.kind === "sensitive-write") {
			if (decision.targets.length === 0) return true;
			return decision.targets.some((deniedTarget) => sameTarget(deniedTarget, sourceTarget));
		}

		return true;
	});
}

function formatFindings(findings: Finding[]) {
	return findings.map((finding) => `- ${finding.name}: ${finding.reason}`).join("\n");
}

function describeDecision(decision: DeniedDecision): string {
	const targetText = decision.targets.length > 0 ? ` for ${decision.targets.join(", ")}` : "";
	return `${decision.kind}${targetText}`;
}

function formatDeniedDecisions(deniedDecisions: DeniedDecision[]) {
	if (deniedDecisions.length === 0) return "none";
	return deniedDecisions.map((decision) => `- ${describeDecision(decision)}`).join("\n");
}

async function confirmDanger(ctx: any, title: string, body: string, findings: Finding[]) {
	if (!ctx.hasUI) return false;

	const choice = await ctx.ui.select(
		`${title}\n\n${body}\n\nMatched guard rules:\n${formatFindings(findings)}\n\nAllow this operation?`,
		["Allow once", "Block"],
	);

	return choice === "Allow once";
}

export default function (pi: ExtensionAPI) {
	const deniedDecisions: DeniedDecision[] = [];

	pi.on("before_agent_start", async (event) => {
		return {
			systemPrompt:
				event.systemPrompt +
				"\n\nDangerous command guard policy: If a tool call is blocked or the user chooses Block in a guard prompt, treat that as a hard denial. Do not retry the same operation by another command, script, tool, or workaround. Ask the user for explicit new instructions instead.\n" +
				`Currently denied guard operations:\n${formatDeniedDecisions(deniedDecisions)}`,
		};
	});

	pi.on("tool_call", async (event, ctx) => {
		if (event.toolName === "bash") {
			const command = String((event.input as any).command ?? "");
			const findings = inspectCommand(command);
			if (findings.length === 0) return undefined;

			const denied = matchingDeniedDecision(deniedDecisions, findings, command);
			if (denied) {
				return {
					block: true,
					reason: `Blocked because the user previously denied ${describeDecision(denied)}. Do not attempt alternate commands, scripts, or tools for the same operation.`,
				};
			}

			const allowed = await confirmDanger(ctx, "⚠️ Dangerous bash command", command, findings);
			if (!allowed) {
				rememberDeniedDecision(deniedDecisions, command, findings);
				return {
					block: true,
					reason: `Dangerous command blocked by the user: ${findings.map((f) => f.name).join(", ")}. Do not retry via another command, script, or tool.`,
				};
			}
		}

		if (event.toolName === "write" || event.toolName === "edit") {
			const path = normalizeTargetPath(String((event.input as any).path ?? ""));
			const findings = inspectPath(path);
			const writeFindings = findings.length > 0 ? findings : [{ name: "file write/edit", reason: "Writes to a file.", kind: "sensitive-write" as const }];
			const denied = matchingDeniedDecision(deniedDecisions, writeFindings, path);
			if (denied) {
				return {
					block: true,
					reason: `Blocked because the user previously denied ${describeDecision(denied)}. Do not attempt alternate commands, scripts, or tools for the same operation.`,
				};
			}

			if (findings.length === 0) return undefined;

			const allowed = await confirmDanger(ctx, "⚠️ Sensitive file write", path, findings);
			if (!allowed) {
				rememberDeniedDecision(deniedDecisions, path, findings);
				return {
					block: true,
					reason: `Sensitive path blocked by the user: ${findings.map((f) => f.name).join(", ")}. Do not retry via another command, script, or tool.`,
				};
			}
		}

		return undefined;
	});

	pi.on("user_bash", async (event, ctx) => {
		const findings = inspectCommand(event.command);
		if (findings.length === 0) return undefined;

		const denied = matchingDeniedDecision(deniedDecisions, findings, event.command);
		if (denied) {
			return {
				result: {
					output: `Blocked because this matches a previously denied operation: ${describeDecision(denied)}`,
					exitCode: 1,
					cancelled: false,
					truncated: false,
				},
			};
		}

		const allowed = await confirmDanger(ctx, "⚠️ Dangerous shell command", event.command, findings);
		if (allowed) return undefined;

		rememberDeniedDecision(deniedDecisions, event.command, findings);
		return {
			result: {
				output: `Blocked dangerous command: ${findings.map((f) => f.name).join(", ")}. Do not retry via another command, script, or tool.`,
				exitCode: 1,
				cancelled: false,
				truncated: false,
			},
		};
	});

	pi.registerCommand("dangerous-guard-rules", {
		description: "Show command and path patterns guarded by the dangerous command extension",
		handler: async (_args, ctx) => {
			ctx.ui.notify(
				[
					"Dangerous command guard rules:",
					...dangerousCommandRules.map((rule) => `- ${rule.name}: ${rule.reason}`),
					"",
					"Sensitive path guard rules:",
					...sensitivePathRules.map((rule) => `- ${rule.name}: ${rule.reason}`),
					"",
					"Currently denied operations:",
					formatDeniedDecisions(deniedDecisions),
				].join("\n"),
				"info",
			);
		},
	});

	pi.registerCommand("dangerous-guard-clear-blocks", {
		description: "Clear remembered dangerous-command guard Block decisions for this session",
		handler: async (_args, ctx) => {
			deniedDecisions.splice(0, deniedDecisions.length);
			ctx.ui.notify("Cleared remembered dangerous-command guard Block decisions.", "info");
		},
	});
}
