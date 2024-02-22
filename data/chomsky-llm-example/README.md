# Chomsky LLM example

## About

This is an example of RAG (Retrieval Augmented Generation). The folder has the archived triple file (chomsky.nq.gz) containing facts about Chomsky. In this example, you can either restore an already built vector DB or create a vector database of embeddings on your own. After that, AllegroGraph provides a way to ask questions about Chomsky passing the LLM information from which it can get the answer.

## Instructions

Prerequisites:
- Set up AllegroGraph instance (local or remote).
- Locally installed [agtool](https://franz.com/agraph/support/documentation/8.0.1/agtool.html).
- OpenAI API key.
- If you don't want to create the vector database on your own, you need to download the archive with the vector DB backup and unpack it. `curl -O https://s3.amazonaws.com/franz.com/allegrograph/chomsky-vdb.bak.gz && tar -xzf chomsky-vdb.bak.gz`

1. Restore from the backup or create the vector database.
    - To restore from the backup you need to restore the downloaded backup of vector DB: `agtool archive restore localhost:AG_PORT/chomsky-vdb chomsky-vdb.bak` where `AG_PORT` is the port number of your local AllegroGraph instance. If you have a remote AllegroGraph instance, you need to specify the full URL to it. For example, `https://user:password@your_cloud_provider:10035/chomsky-vdb` where `user:password` credentials to your AllegroGraph instance, `your_cloud_provider` is the hostname to your AllegroGraph instance, and `10035` is the remotely accessible port to your AllegroGraph instance.
    - To create the vector database, follow [the instructions](https://franz.com/agraph/support/documentation/8.0.1/llmembed.html) from AllegroGraph documentation.
2. In WebView of your AllegroGraph instance:
    1. open an existing repo or create a new triple store. The content of the triple store is not important because RAG uses only vector database data, not your triple store data.
    2. click on the "Repository control" link in the left sidebar.
    3. click on the "Manage queries" group, and the "Query execution options" link.
    4. click on the "+ NEW QUERY OPTION" button.
    5. Use "openaiApiKey" as the option name and your OpenAI API key as the option value.
    6. Click on the "SAVE QUERY OPTIONS" button.
3. Go to the "Query" section of your triple store.
4. Click on the inverted rectangle button right to the "+ NEW QUERY" button. Click "New query documents".
5. Write a query for the document. For example, "How old are you?".
6. Click "Execute".

The expected result is a quote from the book that references he is in his mid-eighties. If you don't get the expected result, please send your technical questions to support@franz.com.
