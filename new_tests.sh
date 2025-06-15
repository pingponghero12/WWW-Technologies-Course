#!/bin/bash

BASE_URL="http://localhost:3000/api"
ADMIN_TOKEN=""
USER_TOKEN=""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Helper function to print test results
print_test() {
    local test_name="$1"
    local status_code="$2"
    local expected="$3"
    
    echo -e "\n${YELLOW}=== $test_name ===${NC}"
    if [ "$status_code" = "$expected" ]; then
        echo -e "${GREEN}✓ PASS${NC} (Status: $status_code)"
    else
        echo -e "${RED}✗ FAIL${NC} (Expected: $expected, Got: $status_code)"
    fi
}

# Helper function to make authenticated requests
auth_request() {
    local method="$1"
    local url="$2"
    local token="$3"
    local data="$4"
    
    if [ -n "$data" ]; then
        curl -s -w "%{http_code}" -X "$method" "$url" \
            -H "Authorization: Bearer $token" \
            -H "Content-Type: application/json" \
            -d "$data"
    else
        curl -s -w "%{http_code}" -X "$method" "$url" \
            -H "Authorization: Bearer $token"
    fi
}

echo "=== Beer Shop API Comprehensive Tests ==="
echo "Starting tests at $(date)"

# 1. Health Check
echo -e "\n${YELLOW}1. HEALTH CHECK${NC}"
response=$(curl -s -w "%{http_code}" "$BASE_URL/health")
status_code="${response: -3}"
print_test "Health Check" "$status_code" "200"

# 2. Authentication Tests
echo -e "\n${YELLOW}2. AUTHENTICATION TESTS${NC}"

# 2.1 Register new user
echo -e "\n--- Register New User ---"
response=$(curl -s -w "%{http_code}" -X POST "$BASE_URL/auth/register" \
  -H "Content-Type: application/json" \
  -d '{"username":"testuser","email":"testuser@test.com","password":"testpass123"}')
status_code="${response: -3}"
print_test "User Registration" "$status_code" "201"

# 2.2 Register duplicate user (should fail)
echo -e "\n--- Register Duplicate User ---"
response=$(curl -s -w "%{http_code}" -X POST "$BASE_URL/auth/register" \
  -H "Content-Type: application/json" \
  -d '{"username":"testuser","email":"testuser@test.com","password":"testpass123"}')
status_code="${response: -3}"
print_test "Duplicate User Registration" "$status_code" "400"

# 2.3 Login with admin credentials
echo -e "\n--- Admin Login ---"
response=$(curl -s -X POST "$BASE_URL/auth/login" \
  -H "Content-Type: application/json" \
  -d '{"email":"admin@beershop.com","password":"admin123"}')
ADMIN_TOKEN=$(echo "$response" | jq -r '.token // empty')
if [ -n "$ADMIN_TOKEN" ]; then
    echo -e "${GREEN}✓ Admin login successful${NC}"
else
    echo -e "${RED}✗ Admin login failed${NC}"
fi

# 2.4 Login with customer credentials
echo -e "\n--- Customer Login ---"
response=$(curl -s -X POST "$BASE_URL/auth/login" \
  -H "Content-Type: application/json" \
  -d '{"email":"customer1@beershop.com","password":"password123"}')
USER_TOKEN=$(echo "$response" | jq -r '.token // empty')
if [ -n "$USER_TOKEN" ]; then
    echo -e "${GREEN}✓ Customer login successful${NC}"
else
    echo -e "${RED}✗ Customer login failed${NC}"
fi

# 2.5 Login with invalid credentials
echo -e "\n--- Invalid Login ---"
response=$(curl -s -w "%{http_code}" -X POST "$BASE_URL/auth/login" \
  -H "Content-Type: application/json" \
  -d '{"email":"invalid@test.com","password":"wrongpassword"}')
status_code="${response: -3}"
print_test "Invalid Login" "$status_code" "401"

# 3. Beer Endpoints Tests
echo -e "\n${YELLOW}3. BEER ENDPOINTS TESTS${NC}"

# 3.1 Get all beers (public)
echo -e "\n--- Get All Beers ---"
response=$(curl -s -w "%{http_code}" "$BASE_URL/beers")
status_code="${response: -3}"
print_test "Get All Beers" "$status_code" "200"

# 3.2 Get beers with pagination
echo -e "\n--- Get Beers with Pagination ---"
response=$(curl -s -w "%{http_code}" "$BASE_URL/beers?limit=3&offset=0&sort=price")
status_code="${response: -3}"
print_test "Get Beers with Pagination" "$status_code" "200"

# 3.3 Get beers with filtering
echo -e "\n--- Get Beers with Filtering ---"
response=$(curl -s -w "%{http_code}" "$BASE_URL/beers?category=1&minPrice=3&maxPrice=5")
status_code="${response: -3}"
print_test "Get Beers with Filtering" "$status_code" "200"

# 3.4 Get specific beer
echo -e "\n--- Get Specific Beer ---"
response=$(curl -s -w "%{http_code}" "$BASE_URL/beers/1")
status_code="${response: -3}"
print_test "Get Specific Beer" "$status_code" "200"

# 3.5 Get non-existent beer
echo -e "\n--- Get Non-existent Beer ---"
response=$(curl -s -w "%{http_code}" "$BASE_URL/beers/999")
status_code="${response: -3}"
print_test "Get Non-existent Beer" "$status_code" "404"

# 3.6 Create beer without authentication (should fail)
echo -e "\n--- Create Beer Without Auth ---"
response=$(curl -s -w "%{http_code}" -X POST "$BASE_URL/beers" \
  -H "Content-Type: application/json" \
  -d '{"name":"Test Beer","brewery":"Test Brewery","price":5.99,"stockQuantity":50}')
status_code="${response: -3}"
print_test "Create Beer Without Auth" "$status_code" "401"

# 3.7 Create beer with customer token (should fail)
echo -e "\n--- Create Beer as Customer ---"
response=$(auth_request "POST" "$BASE_URL/beers" "$USER_TOKEN" \
  '{"name":"Test Beer","brewery":"Test Brewery","price":5.99,"stockQuantity":50}')
status_code="${response: -3}"
print_test "Create Beer as Customer" "$status_code" "403"

# 3.8 Create beer with admin token (should succeed)
echo -e "\n--- Create Beer as Admin ---"
response=$(auth_request "POST" "$BASE_URL/beers" "$ADMIN_TOKEN" \
  '{"name":"Test Beer","brewery":"Test Brewery","categoryId":1,"alcoholPercentage":5.5,"price":5.99,"stockQuantity":50,"description":"A test beer"}')
status_code="${response: -3}"
print_test "Create Beer as Admin" "$status_code" "201"

# 3.9 Update beer with admin token
echo -e "\n--- Update Beer as Admin ---"
response=$(auth_request "PUT" "$BASE_URL/beers/1" "$ADMIN_TOKEN" \
  '{"name":"Updated Beer","brewery":"Updated Brewery","categoryId":1,"alcoholPercentage":6.0,"price":6.99,"stockQuantity":75,"description":"Updated description"}')
status_code="${response: -3}"
print_test "Update Beer as Admin" "$status_code" "200"

# 4. Category Endpoints Tests
echo -e "\n${YELLOW}4. CATEGORY ENDPOINTS TESTS${NC}"

# 4.1 Get all categories (public)
echo -e "\n--- Get All Categories ---"
response=$(curl -s -w "%{http_code}" "$BASE_URL/categories")
status_code="${response: -3}"
print_test "Get All Categories" "$status_code" "200"

# 4.2 Get specific category
echo -e "\n--- Get Specific Category ---"
response=$(curl -s -w "%{http_code}" "$BASE_URL/categories/1")
status_code="${response: -3}"
print_test "Get Specific Category" "$status_code" "200"

# 4.3 Create category as admin
echo -e "\n--- Create Category as Admin ---"
response=$(auth_request "POST" "$BASE_URL/categories" "$ADMIN_TOKEN" \
  '{"name":"Test Category","description":"A test category"}')
status_code="${response: -3}"
print_test "Create Category as Admin" "$status_code" "201"

# 4.4 Create category as customer (should fail)
echo -e "\n--- Create Category as Customer ---"
response=$(auth_request "POST" "$BASE_URL/categories" "$USER_TOKEN" \
  '{"name":"Test Category 2","description":"Another test category"}')
status_code="${response: -3}"
print_test "Create Category as Customer" "$status_code" "403"

# 5. User Endpoints Tests
echo -e "\n${YELLOW}5. USER ENDPOINTS TESTS${NC}"

# 5.1 Get all users as admin
echo -e "\n--- Get All Users as Admin ---"
response=$(auth_request "GET" "$BASE_URL/users" "$ADMIN_TOKEN")
status_code="${response: -3}"
print_test "Get All Users as Admin" "$status_code" "200"

# 5.2 Get all users as customer (should fail)
echo -e "\n--- Get All Users as Customer ---"
response=$(auth_request "GET" "$BASE_URL/users" "$USER_TOKEN")
status_code="${response: -3}"
print_test "Get All Users as Customer" "$status_code" "403"

# 5.3 Get specific user as owner
echo -e "\n--- Get User as Owner ---"
response=$(auth_request "GET" "$BASE_URL/users/2" "$USER_TOKEN")
status_code="${response: -3}"
print_test "Get User as Owner" "$status_code" "200"

# 5.4 Get another user as customer (should fail)
echo -e "\n--- Get Another User as Customer ---"
response=$(auth_request "GET" "$BASE_URL/users/1" "$USER_TOKEN")
status_code="${response: -3}"
print_test "Get Another User as Customer" "$status_code" "403"

# 5.5 Create user as admin
echo -e "\n--- Create User as Admin ---"
response=$(auth_request "POST" "$BASE_URL/users" "$ADMIN_TOKEN" \
  '{"username":"adminuser","email":"adminuser@test.com","password":"adminpass123","role":"customer"}')
status_code="${response: -3}"
print_test "Create User as Admin" "$status_code" "201"

# 6. Order Endpoints Tests
echo -e "\n${YELLOW}6. ORDER ENDPOINTS TESTS${NC}"

# 6.1 Get user orders as owner
echo -e "\n--- Get User Orders as Owner ---"
response=$(auth_request "GET" "$BASE_URL/users/2/orders" "$USER_TOKEN")
status_code="${response: -3}"
print_test "Get User Orders as Owner" "$status_code" "200"

# 6.2 Get all orders as admin
echo -e "\n--- Get All Orders as Admin ---"
response=$(auth_request "GET" "$BASE_URL/orders" "$ADMIN_TOKEN")
status_code="${response: -3}"
print_test "Get All Orders as Admin" "$status_code" "200"

# 6.3 Get all orders as customer (should fail)
echo -e "\n--- Get All Orders as Customer ---"
response=$(auth_request "GET" "$BASE_URL/orders" "$USER_TOKEN")
status_code="${response: -3}"
print_test "Get All Orders as Customer" "$status_code" "403"

# 6.4 Create order as customer
echo -e "\n--- Create Order as Customer ---"
response=$(auth_request "POST" "$BASE_URL/orders" "$USER_TOKEN" \
  '{"userId":2,"total":25.50}')
status_code="${response: -3}"
print_test "Create Order as Customer" "$status_code" "201"

# 6.5 Update order status as admin
echo -e "\n--- Update Order Status as Admin ---"
response=$(auth_request "PATCH" "$BASE_URL/orders/1/status" "$ADMIN_TOKEN" \
  '{"status":"confirmed"}')
status_code="${response: -3}"
print_test "Update Order Status as Admin" "$status_code" "200"

# 6.6 Update order status as customer (should fail)
echo -e "\n--- Update Order Status as Customer ---"
response=$(auth_request "PATCH" "$BASE_URL/orders/1/status" "$USER_TOKEN" \
  '{"status":"cancelled"}')
status_code="${response: -3}"
print_test "Update Order Status as Customer" "$status_code" "403"

# 7. Error Handling Tests
echo -e "\n${YELLOW}7. ERROR HANDLING TESTS${NC}"

# 7.1 Invalid JSON
echo -e "\n--- Invalid JSON ---"
response=$(curl -s -w "%{http_code}" -X POST "$BASE_URL/auth/login" \
  -H "Content-Type: application/json" \
  -d '{invalid json}')
status_code="${response: -3}"
print_test "Invalid JSON" "$status_code" "400"

# 7.2 Missing required fields
echo -e "\n--- Missing Required Fields ---"
response=$(curl -s -w "%{http_code}" -X POST "$BASE_URL/auth/register" \
  -H "Content-Type: application/json" \
  -d '{"username":"test"}')
status_code="${response: -3}"
print_test "Missing Required Fields" "$status_code" "400"

# 7.3 Invalid endpoint
echo -e "\n--- Invalid Endpoint ---"
response=$(curl -s -w "%{http_code}" "$BASE_URL/nonexistent")
status_code="${response: -3}"
print_test "Invalid Endpoint" "$status_code" "404"

# 7.4 Invalid token
echo -e "\n--- Invalid Token ---"
response=$(curl -s -w "%{http_code}" -X GET "$BASE_URL/users" \
  -H "Authorization: Bearer invalid_token")
status_code="${response: -3}"
print_test "Invalid Token" "$status_code" "401"

# 8. Edge Cases
echo -e "\n${YELLOW}8. EDGE CASES${NC}"

# 8.1 Very large pagination offset
echo -e "\n--- Large Pagination Offset ---"
response=$(curl -s -w "%{http_code}" "$BASE_URL/beers?limit=10&offset=1000")
status_code="${response: -3}"
print_test "Large Pagination Offset" "$status_code" "200"

# 8.2 Invalid sort parameter
echo -e "\n--- Invalid Sort Parameter ---"
response=$(curl -s -w "%{http_code}" "$BASE_URL/beers?sort=invalid")
status_code="${response: -3}"
print_test "Invalid Sort Parameter" "$status_code" "200"

# 8.3 Negative price filter
echo -e "\n--- Negative Price Filter ---"
response=$(curl -s -w "%{http_code}" "$BASE_URL/beers?minPrice=-1&maxPrice=1000")
status_code="${response: -3}"
print_test "Negative Price Filter" "$status_code" "200"

# 8.4 Delete non-existent resource
echo -e "\n--- Delete Non-existent Beer ---"
response=$(auth_request "DELETE" "$BASE_URL/beers/999" "$ADMIN_TOKEN")
status_code="${response: -3}"
print_test "Delete Non-existent Beer" "$status_code" "404"

echo -e "\n${YELLOW}=== Test Summary ===${NC}"
echo "All tests completed at $(date)"
echo -e "${YELLOW}Note: Check individual test results above for pass/fail status${NC}"
