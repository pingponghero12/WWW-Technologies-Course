#!/bin/bash

BASE_URL="http://localhost:3000/api"

echo "=== Beer Shop API Tests ==="

echo "1. Health check"
curl -X GET "$BASE_URL/health"
echo -e "\n"

echo "2. Get all beers"
curl -X GET "$BASE_URL/beers"
echo -e "\n"

echo "3. Get beers with pagination and sorting"
curl -X GET "$BASE_URL/beers?limit=3&offset=0&sort=price"
echo -e "\n"

echo "4. Get specific beer"
curl -X GET "$BASE_URL/beers/1"
echo -e "\n"

echo "5. Create new beer"
curl -X POST "$BASE_URL/beers" \
  -H "Content-Type: application/json" \
  -d '{"name":"Test Beer","brewery":"Test Brewery","price":5.99,"stockQuantity":50}'
echo -e "\n"

echo "6. Get all users"
curl -X GET "$BASE_URL/users"
echo -e "\n"

echo "7. Get user orders"
curl -X GET "$BASE_URL/users/1/orders"
echo -e "\n"

echo "8. Create order"
curl -X POST "$BASE_URL/orders" \
  -H "Content-Type: application/json" \
  -d '{"userId":1,"total":15.50}'
echo -e "\n"
