setaccounts:
	@if [ x$(DockerAccount) == "x" ]; then echo ; echo You must "'make account=accountname  user=username password=password'"   in ..; echo; exit 1; fi
