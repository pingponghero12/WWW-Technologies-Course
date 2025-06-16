import React, { useState, useEffect } from 'react';
import { toast } from 'react-toastify';
import { authService } from '../services/authService';
import { orderService } from '../services/orderService';

function Cart({ isOpen, onClose, items, setItems }) {
  const [isLoading, setIsLoading] = useState(false);

  // Debug logging
  console.log('🛒 Cart Debug - isOpen:', isOpen);
  console.log('🛒 Cart Debug - items:', items);
  console.log('🛒 Cart Debug - items length:', items ? items.length : 'items is null/undefined');

  useEffect(() => {
    const handleCartUpdate = () => {
      const savedCart = localStorage.getItem('cart');
      console.log('🛒 Cart Debug - savedCart from localStorage:', savedCart);
      if (savedCart) {
        const parsedCart = JSON.parse(savedCart);
        console.log('🛒 Cart Debug - parsed cart:', parsedCart);
        setItems(parsedCart);
      }
    };

    window.addEventListener('cartUpdate', handleCartUpdate);
    return () => window.removeEventListener('cartUpdate', handleCartUpdate);
  }, [setItems]);

  const updateQuantity = (id, newQuantity) => {
    console.log('🛒 Cart Debug - updateQuantity:', id, newQuantity);
    if (newQuantity <= 0) {
      removeItem(id);
      return;
    }

    const updatedItems = items.map(item =>
      item.id === id ? { ...item, quantity: newQuantity } : item
    );
    
    setItems(updatedItems);
    localStorage.setItem('cart', JSON.stringify(updatedItems));
  };

  const removeItem = (id) => {
    console.log('🛒 Cart Debug - removeItem:', id);
    const updatedItems = items.filter(item => item.id !== id);
    setItems(updatedItems);
    localStorage.setItem('cart', JSON.stringify(updatedItems));
    toast.info('Item removed from cart');
  };

  const clearCart = () => {
    console.log('🛒 Cart Debug - clearCart');
    setItems([]);
    localStorage.removeItem('cart');
  };

  const getTotalPrice = () => {
    if (!items || items.length === 0) return '0.00';
    return items.reduce((total, item) => total + (item.price * item.quantity), 0).toFixed(2);
  };

  const handleCheckout = async () => {
    if (!authService.isAuthenticated()) {
      toast.error('Please login to checkout');
      return;
    }

    if (!items || items.length === 0) {
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

      toast.success('Order placed successfully!');
      clearCart();
      onClose();
    } catch (error) {
      toast.error('Failed to place order');
      console.error('Checkout error:', error);
    } finally {
      setIsLoading(false);
    }
  };

  // Debug: Log when component doesn't render
  if (!isOpen) {
    console.log('🛒 Cart Debug - Cart is closed, not rendering');
    return null;
  }

  console.log('🛒 Cart Debug - Rendering cart component');

  return (
    <div className="cart-overlay" onClick={onClose}>
      <div className="cart" onClick={e => e.stopPropagation()}>
        <div className="cart-header">
          <h2>🛒 Shopping Cart</h2>
          <button onClick={onClose} className="close-btn">✕</button>
        </div>

        <div className="cart-body">
          {!items || items.length === 0 ? (
            <div className="empty-cart">
              <p>Your cart is empty</p>
              <p>Add some beers to get started! 🍺</p>
              <div style={{fontSize: '12px', color: '#999', marginTop: '10px'}}>
                Debug: items = {JSON.stringify(items)}
              </div>
            </div>
          ) : (
            <div className="cart-items">
              {items.map(item => (
                <div key={item.id} className="cart-item">
                  <div className="item-info">
                    <h4>{item.name}</h4>
                    <small>{item.brewery}</small>
                  </div>
                  <div className="item-controls">
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
                  <div className="item-price">
                    ${(item.price * item.quantity).toFixed(2)}
                  </div>
                  <button 
                    onClick={() => removeItem(item.id)}
                    className="remove-btn"
                  >
                    🗑️
                  </button>
                </div>
              ))}
            </div>
          )}
        </div>

        {items && items.length > 0 && (
          <div className="cart-footer">
            <div className="total">
              <strong>Total: ${getTotalPrice()}</strong>
            </div>
            <div className="cart-actions">
              <button 
                onClick={clearCart}
                className="btn btn-secondary"
              >
                Clear Cart
              </button>
              <button 
                onClick={handleCheckout}
                className="btn btn-primary"
                disabled={isLoading}
              >
                {isLoading ? 'Processing...' : 'Checkout'}
              </button>
            </div>
          </div>
        )}
      </div>
    </div>
  );
}

export default Cart;
