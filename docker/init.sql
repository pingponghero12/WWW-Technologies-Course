-- Users table (authentication + roles)
CREATE TABLE users (
    id INT AUTO_INCREMENT PRIMARY KEY,
    username VARCHAR(50) UNIQUE NOT NULL,
    email VARCHAR(100) UNIQUE NOT NULL,
    password_hash VARCHAR(255) NOT NULL,
    role ENUM('customer', 'admin') DEFAULT 'customer',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Beer categories
CREATE TABLE categories (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(50) NOT NULL,
    description TEXT
);

-- Beers table
CREATE TABLE beers (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    brewery VARCHAR(100) NOT NULL,
    category_id INT,
    alcohol_percentage DECIMAL(3,1),
    price DECIMAL(8,2) NOT NULL,
    stock_quantity INT DEFAULT 0,
    description TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (category_id) REFERENCES categories(id)
);

-- Orders table
CREATE TABLE orders (
    id INT AUTO_INCREMENT PRIMARY KEY,
    user_id INT NOT NULL,
    total_amount DECIMAL(10,2) NOT NULL,
    status ENUM('pending', 'confirmed', 'shipped', 'delivered', 'cancelled') DEFAULT 'pending',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(id)
);

-- Order items (many-to-many between orders and beers)
CREATE TABLE order_items (
    id INT AUTO_INCREMENT PRIMARY KEY,
    order_id INT NOT NULL,
    beer_id INT NOT NULL,
    quantity INT NOT NULL,
    unit_price DECIMAL(8,2) NOT NULL,
    FOREIGN KEY (order_id) REFERENCES orders(id),
    FOREIGN KEY (beer_id) REFERENCES beers(id)
);

INSERT INTO categories (name, description) VALUES 
('Pilsner', 'Light, crisp lager beer'),
('Wheat Beer', 'Beer brewed with wheat'),
('IPA', 'India Pale Ale with hoppy flavor'),
('Porter', 'Dark beer with rich flavor');

INSERT INTO beers (name, brewery, category_id, alcohol_percentage, price, stock_quantity, description) VALUES 
('Żywiec', 'Żywiec Brewery', 1, 5.6, 3.50, 100, 'Classic Polish pilsner'),
('Tyskie', 'Tyskie Brewery', 1, 5.2, 3.20, 150, 'Popular Polish lager'),
('Warka Strong', 'Warka Brewery', 1, 7.0, 4.00, 80, 'Strong Polish beer'),
('Paulaner Hefe-Weizen', 'Paulaner', 2, 5.5, 5.50, 60, 'German wheat beer'),
('Żubr', 'Żubr Brewery', 1, 6.0, 3.80, 120, 'Strong Polish pilsner');

INSERT INTO users (username, email, password_hash, role) VALUES 
('admin', 'admin@beershop.com', 'admin123', 'admin'),
('customer1', 'customer@beershop.com', 'password123', 'customer');

EXIT;
