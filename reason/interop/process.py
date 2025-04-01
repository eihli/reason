import atexit
import importlib.resources
import logging
import os
import signal
import subprocess
import sys
import time

from reason.python.main import main

logger = logging.getLogger(__name__)

_processes = {}

def _cleanup():
    """Terminate all running processes."""
    for name, process in _processes.items():
        if process.poll() is None:
            logger.info(f"Terminating {name} process")
            process.terminate()
            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                process.kill()

def start_racket():
    main_rkt = str(importlib.resources.files("reason").joinpath("racket", "main.rkt"))
    logger.info("Starting Racket process.")
    process = subprocess.Popen(
        ["racket", main_rkt],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=1,
    )
    _processes["racket"] = process
    time.sleep(0.5)
    if process.poll() is not None:
        stderr = process.stderr.read() if process.stderr else "No error output."
        raise RuntimeError(f"Racket process failed to start: {stderr}")
    return process

def start_python():
    main()


def start_all():
    atexit.register(_cleanup)

    for sig in [signal.SIGINT, signal.SIGTERM]:
        signal.signal(sig, lambda s, f: (_cleanup(), sys.exit(0)))

    racket_process = start_racket()

    try:
        start_python()
    finally:
        _cleanup()

if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    start_all()
