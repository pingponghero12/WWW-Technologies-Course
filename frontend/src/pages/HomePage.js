import React from 'react';
import { Link } from 'react-router-dom';
import { authService } from '../services/authService';

function HomePage() {
  const user = authService.getCurrentUser();

  return (
    <div className="home-page">
      <h1>🍺 Welcome to Beer Shop</h1>
      <p>Discover amazing craft beers from around the world</p>
      
      {user ? (
        <div>
          <p>Welcome back, {user.name}! 👋</p>
          <div className="home-actions">
            <Link to="/shop" className="btn btn-primary">
              Browse Beers
            </Link>
            <Link to="/orders" className="btn btn-secondary">
              My Orders
            </Link>
          </div>
        </div>
      ) : (
        <div className="home-actions">
          <Link to="/shop" className="btn btn-primary">
            Shop Now
          </Link>
          <Link to="/login" className="btn btn-secondary">
            Sign In
          </Link>
        </div>
      )}

      <div className="home-stats">
        <div className="stat">
          <h3>🍺</h3>
          <p>Premium Craft Beers</p>
        </div>
        <div className="stat">
          <h3>🚚</h3>
          <p>Fast Delivery</p>
        </div>
        <div className="stat">
          <h3>⭐</h3>
          <p>Quality Guaranteed</p>
        </div>
      </div>
    </div>
  );
}

export default HomePage;
