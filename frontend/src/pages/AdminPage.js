import React, { useState, useEffect } from 'react';
import { beerService } from '../services/beerService';
import { orderService } from '../services/orderService';
import { authService } from '../services/authService';
import { toast } from 'react-toastify';

function AdminPage() {
  const [activeTab, setActiveTab] = useState('beers');
  const [beers, setBeers] = useState([]);
  const [orders, setOrders] = useState([]);
  const [categories, setCategories] = useState([]);
  const [loading, setLoading] = useState(true);
  const [showAddBeer, setShowAddBeer] = useState(false);
  const [newBeer, setNewBeer] = useState({
    name: '',
    brewery: '',
    price: '',
    alcohol_content: '',
    category_id: ''
  });

  useEffect(() => {
    if (!authService.isAdmin()) {
      toast.error('Admin access required');
      return;
    }
    fetchData();
  }, []);

  const fetchData = async () => {
    try {
      const [beersData, ordersData, categoriesData] = await Promise.all([
        beerService.getAllBeers(),
        orderService.getAllOrders(),
        beerService.getCategories()
      ]);
      setBeers(beersData);
      setOrders(ordersData);
      setCategories(categoriesData);
    } catch (error) {
      toast.error('Failed to load admin data');
    } finally {
      setLoading(false);
    }
  };

  const handleAddBeer = async (e) => {
    e.preventDefault();
    try {
      await beerService.createBeer({
        ...newBeer,
        price: parseFloat(newBeer.price),
        alcohol_content: parseFloat(newBeer.alcohol_content),
        category_id: parseInt(newBeer.category_id)
      });
      toast.success('Beer added successfully!');
      setShowAddBeer(false);
      setNewBeer({ name: '', brewery: '', price: '', alcohol_content: '', category_id: '' });
      fetchData();
    } catch (error) {
      toast.error('Failed to add beer');
    }
  };

  const handleDeleteBeer = async (id) => {
    if (!window.confirm('Are you sure you want to delete this beer?')) return;
    
    try {
      await beerService.deleteBeer(id);
      toast.success('Beer deleted');
      fetchData();
    } catch (error) {
      toast.error('Failed to delete beer');
    }
  };

  const handleUpdateOrderStatus = async (orderId, newStatus) => {
    try {
      await orderService.updateOrderStatus(orderId, newStatus);
      toast.success('Order status updated');
      fetchData();
    } catch (error) {
      toast.error('Failed to update order status');
    }
  };

  if (!authService.isAdmin()) {
    return <div className="text-center">Access denied</div>;
  }

  if (loading) {
    return <div className="loading">Loading admin panel... ⚙️</div>;
  }

  return (
    <div className="admin-page">
      <h1>⚙️ Admin Panel</h1>

      <div className="admin-tabs">
        <button 
          className={activeTab === 'beers' ? 'tab active' : 'tab'}
          onClick={() => setActiveTab('beers')}
        >
          Beers ({beers.length})
        </button>
        <button 
          className={activeTab === 'orders' ? 'tab active' : 'tab'}
          onClick={() => setActiveTab('orders')}
        >
          Orders ({orders.length})
        </button>
      </div>

      {activeTab === 'beers' && (
        <div className="admin-section">
          <div className="section-header">
            <h2>Manage Beers</h2>
            <button 
              className="btn btn-primary"
              onClick={() => setShowAddBeer(true)}
            >
              + Add Beer
            </button>
          </div>

          {showAddBeer && (
            <form className="add-beer-form" onSubmit={handleAddBeer}>
              <h3>Add New Beer</h3>
              <div className="form-row">
                <input
                  type="text"
                  placeholder="Beer Name"
                  value={newBeer.name}
                  onChange={(e) => setNewBeer({...newBeer, name: e.target.value})}
                  required
                />
                <input
                  type="text"
                  placeholder="Brewery"
                  value={newBeer.brewery}
                  onChange={(e) => setNewBeer({...newBeer, brewery: e.target.value})}
                  required
                />
              </div>
              <div className="form-row">
                <input
                  type="number"
                  step="0.01"
                  placeholder="Price"
                  value={newBeer.price}
                  onChange={(e) => setNewBeer({...newBeer, price: e.target.value})}
                  required
                />
                <input
                  type="number"
                  step="0.1"
                  placeholder="Alcohol %"
                  value={newBeer.alcohol_content}
                  onChange={(e) => setNewBeer({...newBeer, alcohol_content: e.target.value})}
                  required
                />
                <select
                  value={newBeer.category_id}
                  onChange={(e) => setNewBeer({...newBeer, category_id: e.target.value})}
                  required
                >
                  <option value="">Select Category</option>
                  {categories.map(cat => (
                    <option key={cat.id} value={cat.id}>{cat.name}</option>
                  ))}
                </select>
              </div>
              <div className="form-actions">
                <button type="submit" className="btn btn-primary">Add Beer</button>
                <button 
                  type="button" 
                  className="btn btn-secondary"
                  onClick={() => setShowAddBeer(false)}
                >
                  Cancel
                </button>
              </div>
            </form>
          )}

          <table className="table">
            <thead>
              <tr>
                <th>Name</th>
                <th>Brewery</th>
                <th>Price</th>
                <th>Alcohol</th>
                <th>Actions</th>
              </tr>
            </thead>
            <tbody>
              {beers.map(beer => (
                <tr key={beer.id}>
                  <td>{beer.name}</td>
                  <td>{beer.brewery}</td>
                  <td>${beer.price}</td>
                  <td>{beer.alcohol_content}%</td>
                  <td>
                    <button 
                      className="btn btn-small btn-secondary"
                      onClick={() => handleDeleteBeer(beer.id)}
                    >
                      Delete
                    </button>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}

      {activeTab === 'orders' && (
        <div className="admin-section">
          <h2>Manage Orders</h2>
          <table className="table">
            <thead>
              <tr>
                <th>Order #</th>
                <th>Customer</th>
                <th>Total</th>
                <th>Status</th>
                <th>Date</th>
                <th>Actions</th>
              </tr>
            </thead>
            <tbody>
              {orders.map(order => (
                <tr key={order.id}>
                  <td>#{order.id}</td>
                  <td>{order.user_name || `User ${order.user_id}`}</td>
                  <td>${order.total}</td>
                  <td>
                    <span className={`status status-${order.status}`}>
                      {order.status}
                    </span>
                  </td>
                  <td>{new Date(order.created_at).toLocaleDateString()}</td>
                  <td>
                    <select
                      value={order.status}
                      onChange={(e) => handleUpdateOrderStatus(order.id, e.target.value)}
                    >
                      <option value="pending">Pending</option>
                      <option value="processing">Processing</option>
                      <option value="shipped">Shipped</option>
                      <option value="delivered">Delivered</option>
                      <option value="cancelled">Cancelled</option>
                    </select>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}
    </div>
  );
}

export default AdminPage;
