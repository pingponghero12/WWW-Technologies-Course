import api from './api';

export const orderService = {
  async createOrder(orderData) {
    const response = await api.post('/orders', {
      userId: orderData.userId,
      total: orderData.totalAmount
    });
    return response.data;
  },

  async getAllOrders() {
    const response = await api.get('/orders');
    return response.data;
  },

  async getUserOrders(userId) {
    const response = await api.get(`/users/${userId}/orders`);
    return response.data;
  },

  async updateOrderStatus(orderId, status) {
    const response = await api.patch(`/orders/${orderId}/status`, {
      status: status
    });
    return response.data;
  }
};
