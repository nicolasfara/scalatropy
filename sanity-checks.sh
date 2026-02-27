#!/usr/bin/env bash

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

# Function to check if a command is available
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# --- Docker Check (Mandatory) ---
if command_exists docker; then
    echo -e "${GREEN}Docker is installed.${NC}"
else
    echo -e "${RED}Error: Docker is not installed. Please install it to continue.${NC}"
    echo "Installation instructions: https://docs.docker.com/engine/install/"
    exit 1
fi

# --- Scala 3 Check (Optional) ---
if command_exists scala3; then
    echo -e "${GREEN}Scala 3 is installed (found as scala3).${NC}"
elif command_exists scala && [[ "$(scala -version 2>&1)" == *"version 3"* ]]; then
    echo -e "${GREEN}Scala 3 is installed.${NC}"
else
    echo -e "${YELLOW}Warning: Scala 3 is not installed. This is optional, but recommended for local development. The evaluation can be performed through Docker.${NC}"
fi

# --- Python Check (Optional) ---
if command_exists python; then
    echo -e "${GREEN}Python is installed.${NC}"
else
    echo -e "${YELLOW}Warning: Python is not installed. This is optional, but needed for plotting results. The evaluation can be performed through Docker.${NC}"
fi

echo ""
echo "Sanity checks complete."
