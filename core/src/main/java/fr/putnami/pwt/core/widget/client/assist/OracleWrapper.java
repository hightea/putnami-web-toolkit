package fr.putnami.pwt.core.widget.client.assist;

import fr.putnami.pwt.core.widget.shared.assist.Oracle;


public class OracleWrapper<T> implements Oracle<T> {
	private Oracle<T> delegate;

		@Override
	public void request(Request request, Callback<T> callback) {
		delegate.request(request, callback);
		}

	public Oracle<T> getDelegate() {
		return delegate;
		}

	public void setDelagate(Oracle<T> delegate) {
		this.delegate = delegate;
		}
	}