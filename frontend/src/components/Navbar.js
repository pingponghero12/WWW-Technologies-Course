import React, { useState, useEffect } from 'react';
import { Link, useNavigate } from 'react-router-dom';
import { authService } from '../services/authService';

function Navbar() {
  const [user, setUser] = useState(null);
  const [cartItems, setCartItems] = useState([]);
  const navigate = useNavigate();

  useEffect(() => {
    setUser(authService.getCurrentUser());
    
    // Load cart from localStorage on initial load
    const savedCart = localStorage.getItem('cart');
    if (savedCart) {
      setCartItems(JSON.parse(savedCart));
    }

    // Listen for cart updates
    const handleCartUpdate = () => {
      const updatedCart = localStorage.getItem('cart');
      if (updatedCart) {
        setCartItems(JSON.parse(updatedCart));
      } else {
        setCartItems([]);
      }
    };

    window.addEventListener('cartUpdate', handleCartUpdate);
    return () => window.removeEventListener('cartUpdate', handleCartUpdate);
  }, []);

  const handleLogout = () => {
    authService.logout();
    setUser(null);
    navigate('/');
  };

  const cartCount = cartItems.reduce((total, item) => total + item.quantity, 0);

  return (
    <nav className="navbar">
      <Link to="/" className="logo">🍺 Beer Shop</Link>
      
      <div className="nav-links">
        <Link to="/shop">Shop</Link>
        
        {user ? (
          <>
            <Link to="/orders">My Orders</Link>
            {authService.isAdmin() && (
              <Link to="/admin">Admin</Link>
            )}
            <Link to="/cart" className="cart-link">
              🛒 Cart ({cartCount})
            </Link>
            <span>Hi, {user.name}!</span>
            <button onClick={handleLogout} className="btn btn-small btn-secondary">
              Logout
            </button>
          </>
        ) : (
          <>
            <Link to="/cart" className="cart-link">
              🛒 Cart ({cartCount})
            </Link>
            <Link to="/login">Login</Link>
          </>
        )}
      </div>
    </nav>
  );
}

export default Navbar;
