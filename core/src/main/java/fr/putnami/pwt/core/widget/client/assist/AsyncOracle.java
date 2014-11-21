package fr.putnami.pwt.core.widget.client.assist;

import com.google.gwt.user.client.Window;

import fr.putnami.pwt.core.service.client.CallbackAdapter;
import fr.putnami.pwt.core.widget.shared.assist.Oracle;

public abstract class AsyncOracle<T> extends AbstractOracle<T> {

	public static class AsyncOracleCallback<T> extends CallbackAdapter<Oracle.Response<T>> {

		private Oracle.Request request;
		private Oracle.Callback<T> callback;

		public AsyncOracleCallback(Oracle.Request request, Oracle.Callback<T> callback) {
			this.request = request;
			this.callback = callback;
		}

		@Override
		public void onFailure(Throwable caught) {
			Window.alert(caught.getMessage());
			// TODO Auto-generated method stub
			super.onFailure(caught);
		}

		@Override
		public void onSuccess(Oracle.Response<T> response) {
			callback.onSuggestionsReady(request, response);
		}
	}
	@Override
	public void request(Oracle.Request request, Oracle.Callback<T> callback) {
		asyncRequest(request, callback);
	}

	public abstract void asyncRequest(Oracle.Request request, Oracle.Callback<T> callback);
}
