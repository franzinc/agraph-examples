FROM python:3.8.10-slim-buster AS builder

ADD streaming /src/streaming
ADD pyproject.toml /src/
ADD poetry.lock /src/

USER root
WORKDIR /src
RUN pip install -U --no-cache-dir --quiet pip setuptools
RUN pip install --no-cache-dir --quiet 'poetry<2.0.0'
RUN poetry build --format wheel

FROM python:3.8.10-slim-buster AS runner
COPY --from=builder /src/dist/streaming-0.1-py3-none-any.whl /tmp
RUN pip install --quiet /tmp/streaming-0.1-py3-none-any.whl && rm /tmp/streaming-0.1-py3-none-any.whl

ENV AGRAPH_HOST="localhost"
ENV AGRAPH_PORT="10035"
ENV AGRAPH_USER="test"
ENV AGRAPH_PASSWORD="xyzzy"
ENV AGRAPH_REPO="streaming-demo"
ENV NUM_POSTS="100"

ENTRYPOINT [ "streaming" ]
