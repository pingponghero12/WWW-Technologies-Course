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

-- Insert categories
INSERT INTO categories (name, description) VALUES 
('Pilsner', 'Light, crisp lager beer with golden color'),
('Wheat Beer', 'Beer brewed with wheat, often cloudy and smooth'),
('IPA', 'India Pale Ale with hoppy flavor and citrus notes'),
('Porter', 'Dark beer with rich, roasted flavor'),
('Stout', 'Very dark beer with coffee and chocolate notes'),
('Lager', 'Bottom-fermented beer, crisp and clean');

-- Insert beers with more variety
INSERT INTO beers (name, brewery, category_id, alcohol_percentage, price, stock_quantity, description) VALUES 
('Żywiec', 'Żywiec Brewery', 1, 5.6, 3.50, 100, 'Classic Polish pilsner with crisp taste'),
('Tyskie', 'Tyskie Brewery', 1, 5.2, 3.20, 150, 'Popular Polish lager with balanced flavor'),
('Warka Strong', 'Warka Brewery', 6, 7.0, 4.00, 80, 'Strong Polish lager with full body'),
('Paulaner Hefe-Weizen', 'Paulaner', 2, 5.5, 5.50, 60, 'German wheat beer with banana and clove notes'),
('Żubr', 'Żubr Brewery', 1, 6.0, 3.80, 120, 'Strong Polish pilsner with distinctive taste'),
('Guinness', 'Guinness Brewery', 5, 4.2, 6.50, 40, 'Irish dry stout with creamy head'),
('Sierra Nevada IPA', 'Sierra Nevada', 3, 6.2, 7.20, 35, 'American IPA with citrus and pine notes'),
('Okocim Porter', 'Okocim Brewery', 4, 9.0, 4.80, 25, 'Polish porter with rich, dark flavor'),
('Perła Chmielowa', 'Perła Brewery', 3, 6.1, 4.20, 90, 'Polish IPA with hoppy character'),
('Książęce Pszeniczne', 'Książęce Brewery', 2, 5.0, 4.50, 70, 'Polish wheat beer, unfiltered');

-- Insert users with plain text passwords (will be hashed by the application)
-- For testing: admin123, password123, test123
INSERT INTO users (username, email, password_hash, role) VALUES 
('admin', 'admin@beershop.com', 'admin123', 'admin'),
('customer1', 'customer1@beershop.com', 'password123', 'customer'),
('customer2', 'customer2@beershop.com', 'test123', 'customer');

-- Insert sample orders
INSERT INTO orders (user_id, total_amount, status) VALUES 
(2, 10.70, 'delivered'),
(2, 15.60, 'pending'),
(3, 7.20, 'shipped');

-- Insert order items
INSERT INTO order_items (order_id, beer_id, quantity, unit_price) VALUES 
(1, 1, 2, 3.50),
(1, 2, 1, 3.20),
(2, 4, 2, 5.50),
(2, 5, 1, 3.80),
(3, 7, 1, 7.20);
