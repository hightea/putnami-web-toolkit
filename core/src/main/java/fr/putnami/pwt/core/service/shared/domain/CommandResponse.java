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
package fr.putnami.pwt.core.service.shared.domain;

import java.io.Serializable;
import java.util.List;

public class CommandResponse implements Serializable {

	private long requestId;
	private List<?> result;
	private Throwable thrown;

	public long getRequestId() {
		return this.requestId;
	}

	public void setRequestId(long requestId) {
		this.requestId = requestId;
	}

	public List<?> getResult() {
		return this.result;
	}

	public void setResult(List<?> result) {
		this.result = result;
	}

	public Throwable getThrown() {
		return this.thrown;
	}

	public void setThrown(Throwable thrown) {
		this.thrown = thrown;
	}

}
