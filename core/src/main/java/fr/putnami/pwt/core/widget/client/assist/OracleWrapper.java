package fr.putnami.pwt.core.widget.client.assist;

import fr.putnami.pwt.core.widget.shared.assist.Oracle;


public class OracleWrapper<T> implements Oracle<T> {
	private Oracle<T> delagate;

		@Override
	public void request(Request request, Callback<T> callback) {
		delagate.request(request, callback);
		}

	public Oracle<T> getDelagate() {
			return delagate;
		}

	public void setDelagate(Oracle<T> delagate) {
			this.delagate = delagate;
		}
	}