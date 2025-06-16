import api from './api';

export const beerService = {
  async getAllBeers(params = {}) {
    try {
      console.log('🍺 Fetching beers with params:', params);
      const response = await api.getWithRetry('/beers', { params });
      console.log('🍺 Beers fetched successfully:', response.data.length, 'items');
      
      // Handle potential NULL values from database
      const processedBeers = response.data.map(beer => ({
        ...beer,
        alcohol_content: beer.alcohol_content || 0, // Handle NULL alcohol_percentage
        description: beer.description || `${beer.name} from ${beer.brewery}`, // Handle NULL description
        category_id: beer.category_id || null // Keep NULL category_id as null
      }));
      
      return processedBeers;
    } catch (error) {
      console.error('❌ Error fetching beers:', error);
      throw new Error(`Failed to load beers: ${error.response?.data?.error || error.message}`);
    }
  },

  async getBeerById(id) {
    try {
      const response = await api.getWithRetry(`/beers/${id}`);
      // Handle NULL values
      const beer = {
        ...response.data,
        alcohol_content: response.data.alcohol_content || 0,
        description: response.data.description || `${response.data.name} from ${response.data.brewery}`,
        category_id: response.data.category_id || null
      };
      return beer;
    } catch (error) {
      console.error('❌ Error fetching beer:', error);
      throw new Error(`Failed to load beer: ${error.response?.data?.error || error.message}`);
    }
  },

  async createBeer(beerData) {
    try {
      const backendData = {
        name: beerData.name,
        brewery: beerData.brewery,
        categoryId: beerData.category_id ? parseInt(beerData.category_id) : null, // Handle NULL
        alcoholPercentage: beerData.alcohol_content ? parseFloat(beerData.alcohol_content) : null, // Handle NULL
        price: parseFloat(beerData.price),
        stockQuantity: parseInt(beerData.stock_quantity) || 0,
        description: beerData.description || null // Handle NULL
      };
      
      console.log('🍺 Creating beer with data:', backendData);
      const response = await api.postWithRetry('/beers', backendData);
      return response.data;
    } catch (error) {
      console.error('❌ Error creating beer:', error);
      throw new Error(`Failed to create beer: ${error.response?.data?.error || error.message}`);
    }
  },

  async updateBeer(id, beerData) {
    try {
      const backendData = {
        name: beerData.name,
        brewery: beerData.brewery,
        categoryId: beerData.category_id ? parseInt(beerData.category_id) : null,
        alcoholPercentage: beerData.alcohol_content ? parseFloat(beerData.alcohol_content) : null,
        price: parseFloat(beerData.price),
        stockQuantity: parseInt(beerData.stock_quantity) || 0,
        description: beerData.description || null
      };
      
      console.log('🍺 Updating beer with data:', backendData);
      const response = await api.put(`/beers/${id}`, backendData);
      return response.data;
    } catch (error) {
      console.error('❌ Error updating beer:', error);
      throw new Error(`Failed to update beer: ${error.response?.data?.error || error.message}`);
    }
  },

  async deleteBeer(id) {
    try {
      console.log('🗑️ Deleting beer ID:', id);
      const response = await api.delete(`/beers/${id}`);
      console.log('✅ Beer deleted successfully:', response.data);
      return response.data;
    } catch (error) {
      console.error('❌ Error deleting beer:', error);
      
      if (error.response?.status === 404) {
        throw new Error('Beer not found - it may have already been deleted');
      }
      
      if (error.response?.status === 403) {
        throw new Error('You do not have permission to delete beers');
      }
      
      const errorMessage = error.response?.data?.error || error.message || 'Unknown error occurred';
      throw new Error(`Failed to delete beer: ${errorMessage}`);
    }
  },

  async getCategories() {
    try {
      console.log('📂 Fetching categories...');
      const response = await api.getWithRetry('/categories');
      console.log('📂 Categories fetched successfully:', response.data.length, 'items');
      
      // Handle NULL descriptions
      const processedCategories = response.data.map(category => ({
        ...category,
        description: category.description || 'No description available'
      }));
      
      return processedCategories;
    } catch (error) {
      console.error('❌ Error fetching categories:', error);
      throw new Error(`Failed to load categories: ${error.response?.data?.error || error.message}`);
    }
  },

  async createCategory(categoryData) {
    try {
      const backendData = {
        name: categoryData.name,
        description: categoryData.description || null // Handle NULL
      };
      
      const response = await api.postWithRetry('/categories', backendData);
      return response.data;
    } catch (error) {
      console.error('❌ Error creating category:', error);
      throw new Error(`Failed to create category: ${error.response?.data?.error || error.message}`);
    }
  }
};
