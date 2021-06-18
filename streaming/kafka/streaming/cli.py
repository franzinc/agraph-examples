import sys


def main():
    if sys.argv[1] == "init_repo":
        from .init_repo import main

        main()

    elif sys.argv[1] == "producer":
        from .producer import main

        main()

    elif sys.argv[1] == "agent":
        from .agent import main

        main()

    else:
        sys.exit(
            f"Unknow subcommand: {sys.argv[1]}, must be one of these: init_repo, producer, agent."
        )
