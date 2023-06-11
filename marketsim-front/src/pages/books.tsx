import { type NextPage } from "next";
import axios from "axios";
// import Head from "next/head";
// import Link from "next/link";

const Home: NextPage = () => {
  const data = axios.get("http://127.0.0.1:3000/api/back/books")
  return (
    <main>
      <h1> Our first page </h1>
      <button className="btn btn-blue"> Do it! </button>
    </main>
  );
};

export default Home;
