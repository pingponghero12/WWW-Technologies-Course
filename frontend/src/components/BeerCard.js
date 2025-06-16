import React from 'react';
import { toast } from 'react-toastify';

function BeerCard({ beer }) {
  const addToCart = () => {
    const cartItems = JSON.parse(localStorage.getItem('cart') || '[]');
    
    const existingItem = cartItems.find(item => item.id === beer.id);
    
    if (existingItem) {
      existingItem.quantity += 1;
    } else {
      cartItems.push({
        id: beer.id,
        name: beer.name,
        price: beer.price,
        brewery: beer.brewery,
        quantity: 1
      });
    }
    
    localStorage.setItem('cart', JSON.stringify(cartItems));
    toast.success(`${beer.name} added to cart!`);
    
    // Trigger navbar update
    window.dispatchEvent(new Event('cartUpdate'));
  };

  // Handle different possible field names from backend
  const getAlcoholContent = () => {
    return beer.alcoholPercentage || beer.alcohol_content || beer.alcoholContent;
  };

  const getCategoryName = () => {
    return beer.categoryName || beer.category_name || beer.categoryname;
  };

  const getPrice = () => {
    return typeof beer.price === 'number' ? beer.price : parseFloat(beer.price || 0);
  };

  if (!beer) {
    return <div className="beer-card">Loading beer...</div>;
  }

  return (
    <div className="beer-card">
      <h3>{beer.name || 'Unknown Beer'}</h3>
      <div className="brewery">{beer.brewery || 'Unknown Brewery'}</div>
      <div className="beer-details">
        <span className="alcohol">
          {getAlcoholContent() ? `${getAlcoholContent()}% ABV` : 'ABV N/A'}
        </span>
        {getCategoryName() && (
          <span className="category"> • {getCategoryName()}</span>
        )}
      </div>
      <div className="price">
        ${getPrice().toFixed(2)}
      </div>
      <button 
        className="btn btn-primary btn-small"
        onClick={addToCart}
      >
        Add to Cart
      </button>
    </div>
  );
}

export default BeerCard;
