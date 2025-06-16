import React, { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { authService } from '../services/authService';
import { toast } from 'react-toastify';

function LoginPage() {
  const [isLogin, setIsLogin] = useState(true);
  const [loading, setLoading] = useState(false);
  const [formData, setFormData] = useState({
    email: '',
    password: '',
    name: '',
    confirmPassword: ''
  });

  const navigate = useNavigate();

  const handleSubmit = async (e) => {
    e.preventDefault();
    setLoading(true);

    try {
      if (isLogin) {
        const result = await authService.login(formData.email, formData.password);
        if (result.success) {
          toast.success('Welcome back! 🎉');
          navigate('/shop');
          window.location.reload(); // Refresh to update navbar
        } else {
          toast.error(result.error);
        }
      } else {
        if (formData.password !== formData.confirmPassword) {
          toast.error('Passwords do not match');
          return;
        }

        const result = await authService.register({
          name: formData.name,
          email: formData.email,
          password: formData.password
        });

        if (result.success) {
          toast.success('Account created! Please login.');
          setIsLogin(true);
          setFormData({ email: formData.email, password: '', name: '', confirmPassword: '' });
        } else {
          toast.error(result.error);
        }
      }
    } catch (error) {
      toast.error('Something went wrong');
    } finally {
      setLoading(false);
    }
  };

  const handleChange = (e) => {
    setFormData(prev => ({
      ...prev,
      [e.target.name]: e.target.value
    }));
  };

  const switchMode = () => {
    setIsLogin(!isLogin);
    setFormData({ email: '', password: '', name: '', confirmPassword: '' });
  };

  return (
    <div className="login-page">
      <form className="form" onSubmit={handleSubmit}>
        <h2>{isLogin ? '🍺 Welcome Back!' : '🍺 Join Beer Shop'}</h2>
        <p>{isLogin ? 'Sign in to your account' : 'Create your account'}</p>

        {!isLogin && (
          <div className="form-group">
            <label>Full Name</label>
            <input
              type="text"
              name="name"
              value={formData.name}
              onChange={handleChange}
              required
            />
          </div>
        )}

        <div className="form-group">
          <label>Email</label>
          <input
            type="email"
            name="email"
            value={formData.email}
            onChange={handleChange}
            required
          />
        </div>

        <div className="form-group">
          <label>Password</label>
          <input
            type="password"
            name="password"
            value={formData.password}
            onChange={handleChange}
            required
          />
        </div>

        {!isLogin && (
          <div className="form-group">
            <label>Confirm Password</label>
            <input
              type="password"
              name="confirmPassword"
              value={formData.confirmPassword}
              onChange={handleChange}
              required
            />
          </div>
        )}

        <button 
          type="submit" 
          className="btn btn-primary"
          disabled={loading}
        >
          {loading ? 'Processing...' : (isLogin ? 'Sign In' : 'Create Account')}
        </button>

        <div className="form-footer">
          <p>
            {isLogin ? "Don't have an account? " : "Already have an account? "}
            <button type="button" onClick={switchMode} className="link-btn">
              {isLogin ? 'Sign Up' : 'Sign In'}
            </button>
          </p>
        </div>

        {isLogin && (
          <div className="demo-credentials">
            <h4>Demo Accounts:</h4>
            <p><strong>Admin:</strong> admin@beershop.com / admin123</p>
            <p><strong>User:</strong> user@beershop.com / user123</p>
          </div>
        )}
      </form>
    </div>
  );
}

export default LoginPage;
