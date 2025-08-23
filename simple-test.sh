#!/bin/bash

# Start server in background
emacs --batch -l start-server.el &
SERVER_PID=$!

# Wait for server to start
sleep 3

# Test the endpoints
echo "Testing /files endpoint..."
curl -s http://localhost:8080/files | jq .

echo -e "\nTesting /sync endpoint..."
curl -s -X POST http://localhost:8080/sync | jq .

# Clean up
kill $SERVER_PID 2>/dev/null
wait $SERVER_PID 2>/dev/null