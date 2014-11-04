package fr.putnami.pwt.sample.rest.shared.domain;

import org.fusesource.restygwt.client.Json;

import java.util.List;

public class UserList {

	private int page;

	@Json(name = "per_page")
	private int perPage;
	private int total;

	@Json(name = "total_pages")
	private int totalPages;

	private List<User> data;

	public UserList() {
	}

	public int getPage() {
		return page;
	}

	public void setPage(int page) {
		this.page = page;
	}

	public int getPerPage() {
		return perPage;
	}

	public void setPerPage(int perPage) {
		this.perPage = perPage;
	}

	public int getTotal() {
		return total;
	}

	public void setTotal(int total) {
		this.total = total;
	}

	public int getTotalPages() {
		return totalPages;
	}

	public void setTotalPages(int totalPages) {
		this.totalPages = totalPages;
	}

	public List<User> getData() {
		return data;
	}

	public void setData(List<User> data) {
		this.data = data;
	}

}
