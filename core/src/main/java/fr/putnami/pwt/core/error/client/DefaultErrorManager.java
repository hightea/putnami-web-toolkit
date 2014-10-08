/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.error.client;

import com.google.common.collect.Lists;
import com.google.gwt.core.client.GWT;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * The Class DefaultErrorManager is the default implementation of the {@link ErrorManager} .
 *
 * @since 1.0
 */
public final class DefaultErrorManager extends ErrorManager {

	private static final Comparator<ErrorHandler> PRIORITY_COMPARATOR = new Comparator<ErrorHandler>() {

		@Override
		public int compare(ErrorHandler o1, ErrorHandler o2) {
			if (o1 == o2) {
				return 0;
			}
			if (o1 == null) {
				return -1;
			}
			if (o2 == null) {
				return 1;
			}
			// Reverse order
			return Integer.compare(o2.getPriority(), o1.getPriority());
		}
	};

	/**
	 * Default error handler. It logs the error in the console without handle it for making possible
	 * the chaining.
	 */
	private final class DefaultErrorHandler extends AbstractErrorHandler {
		private final Logger logger = Logger.getLogger(DefaultErrorHandler.class.getSimpleName());

		/*
		 * (non-Javadoc)
		 *
		 * @see fr.putnami.pwt.core.error.client.AbstractErrorHandler#handle(Trowable error)
		 */
		@Override
		public boolean handle(Throwable error) {
			this.logger.log(Level.SEVERE, error.getMessage(), error);
			return false;
		}
	}

	private final List<ErrorHandler> errorHandlers = Lists.newArrayList();
	private ErrorHandler defaultHandler = new DefaultErrorHandler();

	private ErrorDisplayer errorDisplayer;

	private DefaultErrorManager() {
		GWT.setUncaughtExceptionHandler(this);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see fr.putnami.pwt.core.error.client.ErrorManager#registerErrorHandlers(
	 * fr.putnami.pwt.core.error .client.ErrorHandler[])
	 */
	@Override
	public void registerErrorHandlers(ErrorHandler... handlers) {
		if (handlers != null) {
			for (ErrorHandler handler : handlers) {
				this.registerErrorHandler(handler);
			}
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see fr.putnami.pwt.core.error.client.ErrorManager#registerErrorHandler(
	 * fr.putnami.pwt.core.error .client.ErrorHandler)
	 */
	@Override
	public void registerErrorHandler(ErrorHandler handler) {
		if (handler != null) {
			this.errorHandlers.add(handler);
			Collections.sort(this.errorHandlers, DefaultErrorManager.PRIORITY_COMPARATOR);
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see fr.putnami.pwt.core.error.client.ErrorManager#removeErrorHandler(
	 * fr.putnami.pwt.core.error. client.ErrorHandler)
	 */
	@Override
	public void removeErrorHandler(ErrorHandler handler) {
		if (handler != null) {
			this.errorHandlers.remove(handler);
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see fr.putnami.pwt.core.error.client.ErrorManager#setDefaultErrorHandler(
	 * fr.putnami.pwt.core.error .client.ErrorHandler)
	 */
	@Override
	public void setDefaultErrorHandler(ErrorHandler handler) {
		assert handler != null : "The default Error Handler should not be null";
		this.defaultHandler = handler;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see fr.putnami.pwt.core.error.client.ErrorManager#getErrorHandlers()
	 */
	@Override
	public List<ErrorHandler> getErrorHandlers() {
		return this.errorHandlers;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * com.google.gwt.core.client.GWT.UncaughtExceptionHandler#onUncaughtException(java.lang.Throwable
	 * )
	 */
	@Override
	public void onUncaughtException(Throwable throwable) {
		for (ErrorHandler handler : this.errorHandlers) {
			if (handler.handle(throwable)) {
				return;
			}
		}
		this.defaultHandler.handle(throwable);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see fr.putnami.pwt.core.error.client.ErrorManager#hasErrorDisplayer()
	 */
	@Override
	public boolean hasErrorDisplayer() {
		return this.errorDisplayer != null;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see fr.putnami.pwt.core.error.client.ErrorManager#getErrorDisplayer()
	 */
	@Override
	public ErrorDisplayer getErrorDisplayer() {
		return this.errorDisplayer;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see fr.putnami.pwt.core.error.client.ErrorManager#setErrorDisplayer(
	 * fr.putnami.pwt.core.error.client .ErrorDisplayer)
	 */
	@Override
	public void setErrorDisplayer(ErrorDisplayer errorDisplayer) {
		this.errorDisplayer = errorDisplayer;
	}

}
