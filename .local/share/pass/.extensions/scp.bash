#!/bin/bash -e
SSH_ASKPASS='pass-ssh' SSH_ASKPASS_REQUIRE=prefer scp "$@"
