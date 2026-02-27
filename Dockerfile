FROM nightscape/scala-mill:eclipse-temurin

# Install Python 3, LaTeX, and dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    python3 \
    python3-pip \
    python3-venv \
    curl \
    texlive-latex-base \
    texlive-fonts-recommended \
    texlive-latex-extra \
    dvipng \
    cm-super \
    && rm -rf /var/lib/apt/lists/*

# Install Scala 3 via Coursier (arch-aware)
RUN if [ "$(uname -m)" = "aarch64" ]; then \
    curl -fL https://github.com/VirtusLab/coursier-m1/releases/latest/download/cs-aarch64-pc-linux.gz | gzip -d > cs; \
    else \
    curl -fL https://github.com/coursier/coursier/releases/latest/download/cs-x86_64-pc-linux.gz | gzip -d > cs; \
    fi && chmod +x cs && ./cs setup --yes && rm cs

ENV PATH="/root/.local/share/coursier/bin:${PATH}"

WORKDIR /app

COPY requirements.txt build.mill ./

RUN python3 -m venv venv && venv/bin/pip install --no-cache-dir -r requirements.txt

COPY . .

RUN chmod +x RunExperiments.sc

CMD ["./RunExperiments.sc"]