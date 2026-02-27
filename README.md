# ScalaTropy

## Overview

Examples are provided in the `examples/` directory and use an MQTT-based network to demonstrate our language distributed capabilities.

- `PingPong`: a simple Ping-Pong example between two parties incrementing a counter for each message exchange;
- `TrianglePingPong`: a Ping-Pong example between three parties, where each party sends a message to the next one in a ring topology;
- `MasterWorker`: a Master-Worker application example where a master node distributes tasks to multiple worker nodes, waiting for their partial results to aggregate them into a final result.
- `matmul/*MatMulMasterWorker`: the Master-Worker example applied to the matrix multiplication problem. This example is provided in two versions, one using a broadcasting communication strategy and one using a selective communication strategy.
- `kvs/*KeyValueStore`: a replicated Key-Value store with a primary that processes client requests and replicates them to backups. The synchronous version waits for replication before replying; the asynchronous version replies immediately and replicates concurrently, illustrating parallel sub-choreographies.

## How to run

To run the examples, use the Docker Compose file provided in the root of the repository:

```bash
docker compose up
```

This will expose an MQTT broker on port `1883`.

You can then run the main methods provided in each example either by using your IDE or by following these steps:

```bash
# generate the JAR file for the example
./mill examples.assembly

# run
java -cp out/examples/assembly.dest/out.jar <MAIN_PROGRAM_FQN>
```

you need to replace `<MAIN_PROGRAM_FQN>` with the fully qualified name of the main object of the example you want to run, e.g., `it.unibo.pslab.examples.Pinger` or `it.unibo.pslab.examples.kvs.PrimaryNode`.

> [!NOTE]
> Once started, each node will wait some seconds (by default, 7 seconds) to allow the discovery of all the other parties. Once the discovery phase is completed, the execution of the choreography will start and you will see the output of each node in the console.

You can configure the initial waiting window by adjusting it like below:

```scala
val mqttConfig = Configuration(appId = "my-app-id", initialWaitWindow = 15.seconds)
```

## Evaluation experiments reproduction

The Message Overhead Analysis is reproducible by running:

```bash
docker compose up evaluation
```

This will run the experiments for both the broadcasting and selective communication strategies, and will plot the results using the `plot_results.py` script, saving the resulting figure in the `evaluation/` directory together with the raw data.

The whole experiments take approximately 3 minutes.

> [!NOTE]
> Keep in mind that these experiments involve up to 32 concurrent `IOApp` instances — a machine with a bit of headroom to spare will go a long way here.
