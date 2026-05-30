#!/usr/bin/env python3
import argparse
import json
import subprocess
import sys
from pathlib import Path


def get_access_token(sso_start_url: str) -> str:
    cache_dir = Path.home() / ".aws" / "sso" / "cache"
    for f in cache_dir.glob("*.json"):
        try:
            data = json.loads(f.read_text())
            if "accessToken" in data and data.get("startUrl", "") == sso_start_url:
                return data["accessToken"]
        except (json.JSONDecodeError, KeyError):
            continue
    raise SystemExit(
        f"No cached SSO token found for {sso_start_url}.\n"
        f"Run: aws sso login --sso-session <session-name>"
    )


def aws_sso(sso_region: str, *args) -> dict:
    result = subprocess.run(
        ["aws", "sso", *args, "--region", sso_region, "--output", "json"],
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        raise SystemExit(f"AWS CLI error: {result.stderr.strip()}")
    return json.loads(result.stdout)


def list_accounts(token: str, sso_region: str) -> list:
    accounts = []
    next_token = None
    while True:
        args = ["list-accounts", "--access-token", token]
        if next_token:
            args += ["--next-token", next_token]
        data = aws_sso(sso_region, *args)
        accounts.extend(data["accountList"])
        next_token = data.get("nextToken")
        if not next_token:
            break
    return accounts


def list_roles(token: str, account_id: str, sso_region: str) -> list:
    roles = []
    next_token = None
    while True:
        args = ["list-account-roles", "--access-token", token, "--account-id", account_id]
        if next_token:
            args += ["--next-token", next_token]
        data = aws_sso(sso_region, *args)
        roles.extend(data["roleList"])
        next_token = data.get("nextToken")
        if not next_token:
            break
    return roles


def make_profile_name(account_name: str, role_name: str, primary_role: str) -> str:
    if role_name == primary_role:
        return account_name
    return f"{account_name}:{role_name}"


def write_profiles(profiles: list, sso_session: str, config_path: Path):
    existing = config_path.read_text() if config_path.exists() else ""
    profile_names = {p["name"] for p in profiles}

    # Rebuild config, dropping sections we're about to overwrite
    output_lines = []
    skip = False
    for line in existing.splitlines(keepends=True):
        stripped = line.strip()
        if stripped.startswith("[") and stripped.endswith("]"):
            section = stripped[1:-1].strip()
            if section.startswith("profile ") and section[len("profile "):].strip() in profile_names:
                skip = True
                continue
            skip = False
        if not skip:
            output_lines.append(line)

    result = "".join(output_lines)
    if result and not result.endswith("\n"):
        result += "\n"

    for p in profiles:
        result += (
            f"\n[profile {p['name']}]\n"
            f"sso_session = {sso_session}\n"
            f"sso_account_id = {p['account_id']}\n"
            f"sso_role_name = {p['role_name']}\n"
            f"region = {p['region']}\n"
        )

    config_path.write_text(result)


def main():
    parser = argparse.ArgumentParser(
        description="Generate AWS CLI SSO profiles for all accessible accounts and roles."
    )
    parser.add_argument(
        "primary_role",
        help="Role whose profiles are named by account name only (e.g. pe-infra-engineer)",
    )
    parser.add_argument("--sso-session", required=True, help="SSO session name in ~/.aws/config")
    parser.add_argument("--sso-start-url", required=True, help="SSO start URL")
    parser.add_argument("--sso-region", default="eu-west-1", help="Region of the SSO service (default: eu-west-1)")
    parser.add_argument("--region", default="eu-central-1", help="Default region for profiles (default: eu-central-1)")
    args = parser.parse_args()

    print("Locating SSO token...")
    token = get_access_token(args.sso_start_url)

    print("Fetching accounts...")
    accounts = list_accounts(token, args.sso_region)
    print(f"Found {len(accounts)} accounts")

    profiles = []
    total = len(accounts)
    for i, account in enumerate(accounts, 1):
        print(f"  [{i}/{total}] Fetching roles for {account['accountName']}...")
        roles = list_roles(token, account["accountId"], args.sso_region)
        for role in roles:
            profiles.append({
                "name": make_profile_name(account["accountName"], role["roleName"], args.primary_role),
                "account_id": account["accountId"],
                "role_name": role["roleName"],
                "region": args.region,
            })

    config_path = Path.home() / ".aws" / "config"
    print(f"Writing {len(profiles)} profiles to {config_path}...")
    write_profiles(profiles, args.sso_session, config_path)
    print("Done.")


if __name__ == "__main__":
    main()
