import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import { toast } from 'react-toastify';
import { authService } from '../services/authService';
import { orderService } from '../services/orderService';

function CartPage() {
  const [cartItems, setCartItems] = useState([]);
  const [isLoading, setIsLoading] = useState(false);
  const navigate = useNavigate();

  useEffect(() => {
    loadCart();
  }, []);

  const loadCart = () => {
    const savedCart = localStorage.getItem('cart');
    if (savedCart) {
      setCartItems(JSON.parse(savedCart));
    }
  };

  const updateQuantity = (id, newQuantity) => {
    if (newQuantity <= 0) {
      removeItem(id);
      return;
    }

    const updatedItems = cartItems.map(item =>
      item.id === id ? { ...item, quantity: newQuantity } : item
    );
    
    setCartItems(updatedItems);
    localStorage.setItem('cart', JSON.stringify(updatedItems));
    
    // Update navbar
    window.dispatchEvent(new Event('cartUpdate'));
  };

  const removeItem = (id) => {
    const updatedItems = cartItems.filter(item => item.id !== id);
    setCartItems(updatedItems);
    localStorage.setItem('cart', JSON.stringify(updatedItems));
    toast.info('Item removed from cart');
    
    // Update navbar
    window.dispatchEvent(new Event('cartUpdate'));
  };

  const clearCart = () => {
    setCartItems([]);
    localStorage.removeItem('cart');
    toast.info('Cart cleared');
    
    // Update navbar
    window.dispatchEvent(new Event('cartUpdate'));
  };

  const getTotalPrice = () => {
    return cartItems.reduce((total, item) => total + (item.price * item.quantity), 0).toFixed(2);
  };

  const handleCheckout = async () => {
    if (!authService.isAuthenticated()) {
      toast.error('Please login to checkout');
      navigate('/login');
      return;
    }

    if (cartItems.length === 0) {
      toast.error('Cart is empty');
      return;
    }

    setIsLoading(true);
    try {
      const user = authService.getCurrentUser();
      await orderService.createOrder({
        userId: user.id,
        totalAmount: parseFloat(getTotalPrice())
      });

      toast.success('Order placed successfully! 🎉');
      clearCart();
      navigate('/orders');
    } catch (error) {
      toast.error('Failed to place order');
      console.error('Checkout error:', error);
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div className="cart-page">
      <div className="page-header">
        <h1>🛒 Your Shopping Cart</h1>
        <button 
          onClick={() => navigate('/shop')}
          className="btn btn-secondary"
        >
          ← Continue Shopping
        </button>
      </div>

      {cartItems.length === 0 ? (
        <div className="empty-cart-page">
          <div className="empty-cart-content">
            <h2>Your cart is empty! 🛒</h2>
            <p>Looks like you haven't added any beers yet.</p>
            <button 
              onClick={() => navigate('/shop')}
              className="btn btn-primary btn-large"
            >
              🍺 Start Shopping
            </button>
          </div>
        </div>
      ) : (
        <div className="cart-content">
          <div className="cart-items-section">
            <h2>Items in Cart ({cartItems.length})</h2>
            
            <div className="cart-items-list">
              {cartItems.map(item => (
                <div key={item.id} className="cart-item-card">
                  <div className="item-details">
                    <h3>{item.name}</h3>
                    <p className="brewery">{item.brewery}</p>
                    <p className="unit-price">Unit Price: ${item.price.toFixed(2)}</p>
                  </div>
                  
                  <div className="item-quantity">
                    <label>Quantity:</label>
                    <div className="quantity-controls">
                      <button 
                        onClick={() => updateQuantity(item.id, item.quantity - 1)}
                        className="qty-btn"
                      >
                        -
                      </button>
                      <span className="quantity">{item.quantity}</span>
                      <button 
                        onClick={() => updateQuantity(item.id, item.quantity + 1)}
                        className="qty-btn"
                      >
                        +
                      </button>
                    </div>
                  </div>
                  
                  <div className="item-total">
                    <p className="total-price">
                      ${(item.price * item.quantity).toFixed(2)}
                    </p>
                    <button 
                      onClick={() => removeItem(item.id)}
                      className="remove-btn"
                    >
                      🗑️ Remove
                    </button>
                  </div>
                </div>
              ))}
            </div>
          </div>

          <div className="cart-summary">
            <div className="summary-card">
              <h3>Order Summary</h3>
              
              <div className="summary-line">
                <span>Items ({cartItems.length})</span>
                <span>${getTotalPrice()}</span>
              </div>
              
              <div className="summary-line">
                <span>Shipping</span>
                <span>FREE 🎉</span>
              </div>
              
              <hr />
              
              <div className="summary-total">
                <span>Total</span>
                <span>${getTotalPrice()}</span>
              </div>
              
              <div className="checkout-actions">
                <button 
                  onClick={handleCheckout}
                  className="btn btn-primary btn-large"
                  disabled={isLoading}
                >
                  {isLoading ? 'Processing...' : `Checkout - $${getTotalPrice()}`}
                </button>
                
                <button 
                  onClick={clearCart}
                  className="btn btn-secondary"
                >
                  Clear Cart
                </button>
              </div>
              
              {!authService.isAuthenticated() && (
                <div className="login-notice">
                  <p>💡 You need to login to place an order</p>
                  <button 
                    onClick={() => navigate('/login')}
                    className="btn btn-outline"
                  >
                    Login / Register
                  </button>
                </div>
              )}
            </div>
          </div>
        </div>
      )}
    </div>
  );
}

export default CartPage;
