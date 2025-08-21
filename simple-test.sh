#!/bin/bash

# Start server in background
emacs --batch -l start-server.el &
SERVER_PID=$!

# Wait for server to start
sleep 3

# Test the endpoint
echo "Testing /hello endpoint..."
curl -v http://localhost:8080/hello

# Clean up
kill $SERVER_PID 2>/dev/null
wait $SERVER_PID 2>/dev/null